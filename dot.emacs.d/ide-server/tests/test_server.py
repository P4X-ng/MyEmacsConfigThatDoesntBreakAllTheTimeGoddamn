import importlib.util
import io
import json
import os
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


MODULE_PATH = Path(__file__).resolve().parents[1] / "server.py"
SPEC = importlib.util.spec_from_file_location("ide_server_server", MODULE_PATH)
server = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(server)


class ChatManagerTests(unittest.TestCase):
    def test_send_to_llm_without_api_key_returns_helpful_message(self):
        with patch.dict(os.environ, {"OPENAI_API_KEY": ""}, clear=False):
            manager = server.ChatManager()
            response = manager.send_to_llm("default", "hello")

        self.assertIn("No LLM API key configured.", response)
        history = manager.get_conversation("default")
        self.assertEqual([msg["role"] for msg in history], ["user", "assistant"])

    def test_send_to_llm_calls_openai_compatible_endpoint(self):
        mock_response = MagicMock()
        mock_response.read.return_value = json.dumps(
            {"choices": [{"message": {"content": "LLM answer"}}]}
        ).encode("utf-8")

        context_manager = MagicMock()
        context_manager.__enter__.return_value = mock_response
        context_manager.__exit__.return_value = False

        with patch.dict(
            os.environ,
            {
                "OPENAI_API_KEY": "test-key",
                "OPENAI_BASE_URL": "https://api.openai.com/v1",
                "GPTEL_MODEL": "gpt-4o-mini",
            },
            clear=False,
        ):
            manager = server.ChatManager()
            manager.add_message("conv", "system", "You are concise.")

            with patch.object(server.request, "urlopen", return_value=context_manager) as urlopen_mock:
                response = manager.send_to_llm(
                    "conv",
                    "Write a function prototype in C",
                    "Prefer C99 style.",
                )

        self.assertEqual(response, "LLM answer")
        request_obj = urlopen_mock.call_args.args[0]
        timeout_arg = urlopen_mock.call_args.kwargs["timeout"]

        self.assertEqual(request_obj.full_url, "https://api.openai.com/v1/chat/completions")
        self.assertEqual(timeout_arg, server.LLM_REQUEST_TIMEOUT_SECONDS)

        payload = json.loads(request_obj.data.decode("utf-8"))
        self.assertEqual(payload["model"], "gpt-4o-mini")
        self.assertGreaterEqual(len(payload["messages"]), 3)
        self.assertEqual(payload["messages"][0]["role"], "system")
        self.assertIn("Prefer C99 style.", payload["messages"][0]["content"])
        self.assertEqual(payload["messages"][-1]["role"], "user")
        self.assertEqual(payload["messages"][-1]["content"], "Write a function prototype in C")

    def test_send_to_llm_handles_http_errors_gracefully(self):
        http_error = server.error.HTTPError(
            url="https://api.openai.com/v1/chat/completions",
            code=401,
            msg="Unauthorized",
            hdrs=None,
            fp=io.BytesIO(b'{"error":"unauthorized"}'),
        )

        with patch.dict(os.environ, {"OPENAI_API_KEY": "bad-key"}, clear=False):
            manager = server.ChatManager()
            with patch.object(server.request, "urlopen", side_effect=http_error):
                response = manager.send_to_llm("conv", "hello")

        self.assertIn("LLM request failed: LLM backend returned HTTP 401", response)
        history = manager.get_conversation("conv")
        self.assertEqual(history[-1]["role"], "assistant")
        self.assertIn("LLM request failed:", history[-1]["content"])


if __name__ == "__main__":
    unittest.main()
