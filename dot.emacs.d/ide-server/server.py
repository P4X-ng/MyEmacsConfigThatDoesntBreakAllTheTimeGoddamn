#!/usr/bin/env python3
"""
IDE Server - A Python service for Emacs IDE features.

Provides:
- Chat interface with LLM support
- Context management
- Simple HTTP JSON API for Emacs integration
"""

import json
import os
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from typing import Dict, Any, List
import threading
from datetime import datetime


class ChatManager:
    """Manages chat conversations and LLM interactions."""
    
    def __init__(self):
        self.conversations: Dict[str, List[Dict[str, str]]] = {}
        self.api_key = os.environ.get("OPENAI_API_KEY", "")
        self.backend = os.environ.get("GPTEL_BACKEND", "openai")
        self.base_url = os.environ.get("OPENAI_BASE_URL", "https://api.openai.com/v1")
        self.model = os.environ.get("GPTEL_MODEL", "gpt-4o-mini")
    
    def get_conversation(self, conv_id: str = "default") -> List[Dict[str, str]]:
        """Get messages for a conversation."""
        if conv_id not in self.conversations:
            self.conversations[conv_id] = []
        return self.conversations[conv_id]
    
    def add_message(self, conv_id: str, role: str, content: str) -> Dict[str, Any]:
        """Add a message to a conversation."""
        if conv_id not in self.conversations:
            self.conversations[conv_id] = []
        
        message = {
            "role": role,
            "content": content,
            "timestamp": datetime.now().isoformat()
        }
        self.conversations[conv_id].append(message)
        return message
    
    def send_to_llm(self, conv_id: str, message: str, context: str = "") -> str:
        """Send message to LLM and get response."""
        # Add user message
        self.add_message(conv_id, "user", message)
        
        # For now, return a simple response
        # In a full implementation, this would call the actual LLM API
        if not self.api_key:
            response = f"[No API key set. Echo: {message}]"
        else:
            # Placeholder for actual LLM call
            response = f"[LLM Response placeholder for: {message}]"
        
        # Add assistant response
        self.add_message(conv_id, "assistant", response)
        return response
    
    def clear_conversation(self, conv_id: str) -> bool:
        """Clear a conversation."""
        if conv_id in self.conversations:
            del self.conversations[conv_id]
            return True
        return False
    
    def list_conversations(self) -> List[str]:
        """List all conversation IDs."""
        return list(self.conversations.keys())


class ContextManager:
    """Manages context directories and search."""
    
    def __init__(self):
        self.context_dirs: List[str] = []
        default_dir = os.path.expanduser("~/.llm-context")
        if os.path.exists(default_dir):
            self.context_dirs.append(default_dir)
    
    def add_context_dir(self, path: str) -> bool:
        """Add a context directory."""
        expanded = os.path.expanduser(path)
        if os.path.isdir(expanded) and expanded not in self.context_dirs:
            self.context_dirs.append(expanded)
            return True
        return False
    
    def remove_context_dir(self, path: str) -> bool:
        """Remove a context directory."""
        expanded = os.path.expanduser(path)
        if expanded in self.context_dirs:
            self.context_dirs.remove(expanded)
            return True
        return False
    
    def list_context_dirs(self) -> List[str]:
        """List all context directories."""
        return self.context_dirs.copy()
    
    def search_context(self, query: str) -> str:
        """Search context directories for query."""
        # Placeholder implementation
        return f"Search results for '{query}' in {len(self.context_dirs)} directories"


class IDERequestHandler(BaseHTTPRequestHandler):
    """HTTP request handler for IDE server."""
    
    chat_manager = ChatManager()
    context_manager = ContextManager()
    
    def log_message(self, format, *args):
        """Override to provide cleaner logging."""
        sys.stderr.write(f"[{self.log_date_time_string()}] {format % args}\n")
    
    def _send_json_response(self, data: Any, status: int = 200):
        """Send JSON response."""
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())
    
    def _send_error_response(self, message: str, status: int = 400):
        """Send error response."""
        self._send_json_response({"error": message}, status)
    
    def _read_json_body(self) -> Dict[str, Any]:
        """Read and parse JSON body."""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length == 0:
            return {}
        body = self.rfile.read(content_length)
        return json.loads(body.decode())
    
    def do_GET(self):
        """Handle GET requests."""
        if self.path == '/health':
            self._send_json_response({"status": "ok"})
        
        elif self.path == '/chat/conversations':
            conversations = self.chat_manager.list_conversations()
            self._send_json_response({"conversations": conversations})
        
        elif self.path.startswith('/chat/history/'):
            conv_id = self.path.split('/')[-1]
            messages = self.chat_manager.get_conversation(conv_id)
            self._send_json_response({"messages": messages})
        
        elif self.path == '/context/dirs':
            dirs = self.context_manager.list_context_dirs()
            self._send_json_response({"directories": dirs})
        
        else:
            self._send_error_response("Not found", 404)
    
    def do_POST(self):
        """Handle POST requests."""
        try:
            data = self._read_json_body()
        except json.JSONDecodeError:
            self._send_error_response("Invalid JSON")
            return
        
        if self.path == '/chat/send':
            conv_id = data.get('conversation_id', 'default')
            message = data.get('message', '')
            context = data.get('context', '')
            
            if not message:
                self._send_error_response("Message required")
                return
            
            response = self.chat_manager.send_to_llm(conv_id, message, context)
            self._send_json_response({
                "response": response,
                "conversation_id": conv_id
            })
        
        elif self.path == '/chat/clear':
            conv_id = data.get('conversation_id', 'default')
            success = self.chat_manager.clear_conversation(conv_id)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/add':
            path = data.get('path', '')
            if not path:
                self._send_error_response("Path required")
                return
            
            success = self.context_manager.add_context_dir(path)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/remove':
            path = data.get('path', '')
            if not path:
                self._send_error_response("Path required")
                return
            
            success = self.context_manager.remove_context_dir(path)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/search':
            query = data.get('query', '')
            if not query:
                self._send_error_response("Query required")
                return
            
            results = self.context_manager.search_context(query)
            self._send_json_response({"results": results})
        
        else:
            self._send_error_response("Not found", 404)
    
    def do_OPTIONS(self):
        """Handle OPTIONS for CORS."""
        self.send_response(200)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        self.end_headers()


def run_server(host: str = '127.0.0.1', port: int = 9999):
    """Run the IDE server."""
    server = HTTPServer((host, port), IDERequestHandler)
    print(f"IDE Server starting on http://{host}:{port}")
    print(f"Chat endpoint: http://{host}:{port}/chat/send")
    print(f"Health check: http://{host}:{port}/health")
    print("Press Ctrl+C to stop")
    
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down server...")
        server.shutdown()


if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='IDE Server for Emacs')
    parser.add_argument('--host', default='127.0.0.1', help='Host to bind to')
    parser.add_argument('--port', type=int, default=9999, help='Port to bind to')
    
    args = parser.parse_args()
    run_server(args.host, args.port)
