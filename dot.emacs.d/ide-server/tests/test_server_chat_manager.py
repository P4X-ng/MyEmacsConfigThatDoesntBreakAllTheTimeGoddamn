import io
import importlib.util
import json
import os
from pathlib import Path
import unittest
from urllib import error as urllib_error
from unittest.mock import patch


MODULE_PATH = Path(__file__).resolve().parents[1] / "server.py"


def load_server_module():
    spec = importlib.util.spec_from_file_location("ide_server_module", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Unable to load module from {MODULE_PATH}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class FakeResponse:
    def __init__(self, payload: str):
        self._payload = payload.encode("utf-8")

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def read(self):
        return self._payload


class ChatManagerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server_module = load_server_module()

    def test_returns_warning_when_api_key_missing_for_openai(self):
        with patch.dict(os.environ, {"OPENAI_API_KEY": "", "GPTEL_BACKEND": "openai"}, clear=False):
            manager = self.server_module.ChatManager()
            response = manager.send_to_llm("conv-a", "hello")

        self.assertIn("No LLM API key configured", response)
        history = manager.get_conversation("conv-a")
        self.assertEqual(2, len(history))
        self.assertEqual("user", history[0]["role"])
        self.assertEqual("assistant", history[1]["role"])

    def test_posts_openai_compatible_payload_with_context(self):
        with patch.dict(
            os.environ,
            {
                "OPENAI_API_KEY": "test-key",
                "OPENAI_BASE_URL": "https://example.test/v1",
                "GPTEL_MODEL": "gpt-4o-mini",
            },
            clear=False,
        ):
            manager = self.server_module.ChatManager()
            fake_response = FakeResponse(
                json.dumps({"choices": [{"message": {"content": "assistant reply"}}]})
            )
            with patch.object(
                self.server_module.urllib_request, "urlopen", return_value=fake_response
            ) as mocked_urlopen:
                response = manager.send_to_llm("conv-b", "write code", context="repo context")

        self.assertEqual("assistant reply", response)
        request_obj = mocked_urlopen.call_args[0][0]
        payload = json.loads(request_obj.data.decode("utf-8"))
        self.assertEqual("gpt-4o-mini", payload["model"])
        self.assertEqual("system", payload["messages"][0]["role"])
        self.assertIn("repo context", payload["messages"][0]["content"])
        self.assertEqual("user", payload["messages"][-1]["role"])
        self.assertEqual("write code", payload["messages"][-1]["content"])
        self.assertEqual("Bearer test-key", request_obj.get_header("Authorization"))

    def test_uses_local_fallback_token_for_vllm_backend(self):
        with patch.dict(
            os.environ,
            {
                "OPENAI_API_KEY": "",
                "GPTEL_BACKEND": "vllm",
                "OPENAI_BASE_URL": "http://localhost:8000/v1",
            },
            clear=False,
        ):
            manager = self.server_module.ChatManager()
            fake_response = FakeResponse(
                json.dumps({"choices": [{"message": {"content": "local reply"}}]})
            )
            with patch.object(
                self.server_module.urllib_request, "urlopen", return_value=fake_response
            ) as mocked_urlopen:
                response = manager.send_to_llm("conv-c", "hello local")

        self.assertEqual("local reply", response)
        request_obj = mocked_urlopen.call_args[0][0]
        self.assertEqual("Bearer local-llm-token", request_obj.get_header("Authorization"))

    def test_returns_diagnostic_message_when_provider_rejects_request(self):
        with patch.dict(
            os.environ,
            {
                "OPENAI_API_KEY": "bad-key",
                "OPENAI_BASE_URL": "https://example.test/v1",
            },
            clear=False,
        ):
            manager = self.server_module.ChatManager()
            error = urllib_error.HTTPError(
                url="https://example.test/v1/chat/completions",
                code=401,
                msg="Unauthorized",
                hdrs=None,
                fp=io.BytesIO(b'{"error":{"message":"bad key"}}'),
            )
            with patch.object(
                self.server_module.urllib_request, "urlopen", side_effect=error
            ):
                response = manager.send_to_llm("conv-d", "hello")

        self.assertIn("LLM request failed:", response)
        self.assertIn("bad key", response)


if __name__ == "__main__":
    unittest.main()
