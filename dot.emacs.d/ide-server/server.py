#!/usr/bin/env python3
"""
IDE Server - A Python service for Emacs IDE features.

Provides:
- Chat interface with LLM support
- Context management
- Simple HTTP JSON API for Emacs integration
- Rate limiting for security
"""

import json
import os
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from typing import Dict, Any, List
import threading
from datetime import datetime, timedelta
from collections import defaultdict

# Security configuration constants
FORBIDDEN_PATH_PREFIXES = tuple(
    os.environ.get('FORBIDDEN_PATH_PREFIXES', '/etc:/sys:/proc:/dev:/boot').split(':')
)

# Rate limiting configuration from environment
RATE_LIMIT_MAX_REQUESTS = int(os.environ.get('IDE_SERVER_MAX_REQUESTS', '100'))
RATE_LIMIT_WINDOW_SECONDS = int(os.environ.get('IDE_SERVER_RATE_LIMIT_WINDOW', '60'))

# Global shared state with thread safety
_chat_manager = None
_context_manager = None
_rate_limiter = None
_state_lock = threading.Lock()


class RateLimiter:
    """Simple rate limiter to prevent abuse."""
    
    def __init__(self, max_requests: int = 100, window_seconds: int = 60):
        """
        Initialize rate limiter.
        
        Args:
            max_requests: Maximum requests allowed per window
            window_seconds: Time window in seconds
        """
        self.max_requests = max_requests
        self.window_seconds = window_seconds
        self.requests = defaultdict(list)
        self.lock = threading.Lock()
    
    def is_allowed(self, client_id: str) -> bool:
        """
        Check if request from client is allowed.
        
        Args:
            client_id: Client identifier (usually IP address)
            
        Returns:
            True if request is allowed, False if rate limit exceeded
        """
        with self.lock:
            now = datetime.now()
            cutoff = now - timedelta(seconds=self.window_seconds)
            
            # Remove old requests outside the window
            self.requests[client_id] = [
                req_time for req_time in self.requests[client_id]
                if req_time > cutoff
            ]
            
            # Check if under limit
            if len(self.requests[client_id]) < self.max_requests:
                self.requests[client_id].append(now)
                return True
            
            return False
    
    def get_stats(self, client_id: str) -> Dict[str, Any]:
        """Get rate limit stats for a client."""
        with self.lock:
            now = datetime.now()
            cutoff = now - timedelta(seconds=self.window_seconds)
            recent_requests = [
                req_time for req_time in self.requests[client_id]
                if req_time > cutoff
            ]
            return {
                "requests": len(recent_requests),
                "max_requests": self.max_requests,
                "window_seconds": self.window_seconds,
                "remaining": max(0, self.max_requests - len(recent_requests))
            }


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
        
        # Check for API key and provide clear feedback
        if not self.api_key:
            response = (
                "⚠️  No LLM API key configured. "
                "Set OPENAI_API_KEY environment variable to enable chat. "
                f"Your message: {message}"
            )
        else:
            # TODO: Implement actual LLM API call here
            # Example implementations:
            # 
            # For OpenAI (install: pip install openai):
            #   import openai
            #   client = openai.OpenAI(api_key=self.api_key, base_url=self.base_url)
            #   response = client.chat.completions.create(
            #       model=self.model,
            #       messages=self.get_conversation(conv_id)
            #   )
            #   return response.choices[0].message.content
            #
            # For local LLMs (vLLM, TGI compatible):
            #   Same as OpenAI, just set OPENAI_BASE_URL to local endpoint
            #
            # This is a placeholder for the actual implementation
            response = (
                f"🤖 LLM integration placeholder (backend: {self.backend}, model: {self.model})\n"
                f"Your message: {message}\n"
                f"To enable full LLM functionality, implement the API call in server.py (see TODO above)"
            )
        
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
        """Add a context directory with robust security validation."""
        # Security: Basic validation
        if not path or not path.strip():
            return False
        
        try:
            # Normalize and expand the path
            expanded = os.path.abspath(os.path.expanduser(path.strip()))
            normalized = os.path.normpath(expanded)
            
            # Security: Check for directory traversal attempts
            # Ensure the normalized path doesn't contain '..'
            if '..' in os.path.relpath(normalized, '/'):
                return False
            
            # Security: Verify it's a real directory (not symlink to forbidden location)
            if not os.path.isdir(normalized):
                return False
            
            # Security: Prevent duplicate entries
            if normalized in self.context_dirs:
                return False
            
            # Security: Block access to system directories
            if normalized.startswith(FORBIDDEN_PATH_PREFIXES):
                return False
            
            # All checks passed - add the directory
            self.context_dirs.append(normalized)
            return True
            
        except (ValueError, OSError):
            # Handle any path resolution errors
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
        """Search context directories for query using simple grep."""
        # Security: Validate query input
        if not query:
            return "Empty query provided."
        
        query = query.strip()
        
        # Security: Prevent abuse with overly long queries
        if len(query) > 1000:
            return "Query too long. Maximum 1000 characters."
        
        # Security: Block dangerous characters that could be used for command injection
        # if the query were ever passed to shell (defense in depth)
        dangerous_chars = ['\x00', '\n', '\r', ';', '|', '&', '`', '$', '(', ')', '<', '>']
        if any(char in query for char in dangerous_chars):
            return "Invalid characters in query. Alphanumeric and basic punctuation only."
        
        if not self.context_dirs:
            return "No context directories configured. Use /context/add to add directories."
        
        # Extended list of binary/excluded file extensions
        excluded_extensions = (
            '.pyc', '.pyo', '.so', '.o', '.class', '.jar', '.war', '.ear',
            '.exe', '.dll', '.bin', '.dat', '.db', '.sqlite', '.log',
            '.jpg', '.jpeg', '.png', '.gif', '.bmp', '.ico', '.svg',
            '.zip', '.tar', '.gz', '.bz2', '.xz', '.rar', '.7z',
            '.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx'
        )
        
        results = []
        file_match_counts = {}  # Track matches per file for efficiency
        
        for directory in self.context_dirs:
            if not os.path.isdir(directory):
                continue
            
            # Simple file search - walk directory and search files
            try:
                for root, dirs, files in os.walk(directory):
                    # Skip hidden directories
                    dirs[:] = [d for d in dirs if not d.startswith('.')]
                    
                    for file in files:
                        # Skip hidden files and binary files
                        if file.startswith('.') or file.endswith(excluded_extensions):
                            continue
                        
                        filepath = os.path.join(root, file)
                        file_match_counts[filepath] = 0
                        
                        try:
                            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                                for line_num, line in enumerate(f, 1):
                                    if query.lower() in line.lower():
                                        results.append(f"{filepath}:{line_num}: {line.strip()}")
                                        file_match_counts[filepath] += 1
                                        
                                        # Limit results per file
                                        if file_match_counts[filepath] >= 3:
                                            break
                        except (IOError, OSError):
                            continue
                        
                        # Limit total results
                        if len(results) >= 20:
                            break
                    if len(results) >= 20:
                        break
            except (IOError, OSError) as e:
                results.append(f"Error searching {directory}: {e}")
        
        if not results:
            return f"No matches found for '{query}' in {len(self.context_dirs)} context directories."
        
        return "\n".join(results[:20])  # Return top 20 results


class IDERequestHandler(BaseHTTPRequestHandler):
    """HTTP request handler for IDE server."""
    
    @property
    def chat_manager(self):
        """Get shared chat manager instance."""
        global _chat_manager
        if _chat_manager is None:
            with _state_lock:
                if _chat_manager is None:
                    _chat_manager = ChatManager()
        return _chat_manager
    
    @property
    def context_manager(self):
        """Get shared context manager instance."""
        global _context_manager
        if _context_manager is None:
            with _state_lock:
                if _context_manager is None:
                    _context_manager = ContextManager()
        return _context_manager
    
    @property
    def rate_limiter(self):
        """Get shared rate limiter instance."""
        global _rate_limiter
        if _rate_limiter is None:
            with _state_lock:
                if _rate_limiter is None:
                    # Use configuration from environment variables
                    _rate_limiter = RateLimiter(
                        max_requests=RATE_LIMIT_MAX_REQUESTS,
                        window_seconds=RATE_LIMIT_WINDOW_SECONDS
                    )
        return _rate_limiter
    
    def _check_rate_limit(self) -> bool:
        """Check if request should be rate limited."""
        client_ip = self.client_address[0]
        if not self.rate_limiter.is_allowed(client_ip):
            stats = self.rate_limiter.get_stats(client_ip)
            self._send_error_response(
                f"Rate limit exceeded. Try again in {stats['window_seconds']} seconds.",
                429
            )
            return False
        return True
    
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
        # Security: Check rate limit for all POST requests
        if not self._check_rate_limit():
            return
        
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
