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
import logging
from pathlib import Path

# Module logger - configure at module level for simple server
logger = logging.getLogger(__name__)

# Constants for security and performance
MAX_FILE_SIZE = 1024 * 1024  # 1MB max file size to read
MAX_SEARCH_RESULTS = 20  # Maximum search results to return
MAX_RESULTS_PER_FILE = 3  # Maximum matches per file
MAX_DIRECTORY_DEPTH = 5  # Maximum directory traversal depth
MAX_MESSAGE_LENGTH = 10000  # Maximum message length
MAX_CONTEXT_DIRS = 10  # Maximum number of context directories
MAX_REQUEST_SIZE = 1024 * 1024  # 1MB max HTTP request body size
MAX_FILES_TO_PROCESS = 100  # Maximum files to process in search

# Global shared state with thread safety
_chat_manager = None
_context_manager = None
_state_lock = threading.Lock()


class ChatManager:
    """Manages chat conversations and LLM interactions."""
    
    def __init__(self):
        self.conversations: Dict[str, List[Dict[str, str]]] = {}
        self.api_key = os.environ.get("OPENAI_API_KEY", "")
        self.backend = os.environ.get("GPTEL_BACKEND", "openai")
        self.base_url = os.environ.get("OPENAI_BASE_URL", "https://api.openai.com/v1")
        self.model = os.environ.get("GPTEL_MODEL", "gpt-4o-mini")
        logger.info(f"ChatManager initialized with backend: {self.backend}, model: {self.model}")
    
    def get_conversation(self, conv_id: str = "default") -> List[Dict[str, str]]:
        """Get messages for a conversation."""
        if conv_id not in self.conversations:
            self.conversations[conv_id] = []
        return self.conversations[conv_id]
    
    def add_message(self, conv_id: str, role: str, content: str) -> Dict[str, Any]:
        """Add a message to a conversation."""
        if conv_id not in self.conversations:
            self.conversations[conv_id] = []
        
        # Validate role
        if role not in ["user", "assistant", "system"]:
            raise ValueError(f"Invalid role: {role}")
        
        # Validate content length
        if len(content) > MAX_MESSAGE_LENGTH:
            raise ValueError(f"Message exceeds maximum length of {MAX_MESSAGE_LENGTH} characters")
        
        message = {
            "role": role,
            "content": content,
            "timestamp": datetime.now().isoformat()
        }
        self.conversations[conv_id].append(message)
        return message
    
    def send_to_llm(self, conv_id: str, message: str, context: str = "") -> str:
        """Send message to LLM and get response."""
        # Validate message length
        if len(message) > MAX_MESSAGE_LENGTH:
            raise ValueError(f"Message exceeds maximum length of {MAX_MESSAGE_LENGTH} characters")
        
        # Add user message
        self.add_message(conv_id, "user", message)
        
        # Check for API key and provide clear feedback
        if not self.api_key:
            response = (
                "⚠️  No LLM API key configured. "
                "Set OPENAI_API_KEY environment variable to enable chat. "
                f"Your message: {message}"
            )
            logger.warning("LLM API call attempted without API key")
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
            logger.info(f"Placeholder LLM response generated for conversation: {conv_id}")
        
        # Add assistant response
        self.add_message(conv_id, "assistant", response)
        return response
    
    def clear_conversation(self, conv_id: str) -> bool:
        """Clear a conversation."""
        if conv_id in self.conversations:
            del self.conversations[conv_id]
            logger.info(f"Conversation cleared: {conv_id}")
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
        logger.info(f"ContextManager initialized with {len(self.context_dirs)} directories")
    
    def _is_safe_path(self, path: str, must_be_dir: bool = True) -> bool:
        """
        Validate that a path is safe to access.
        
        Prevents:
        - Path traversal attacks (../)
        - Access to system directories
        - Symbolic link attacks
        
        Args:
            path: The path to validate
            must_be_dir: If True, path must be a directory
            
        Returns:
            True if the path is safe, False otherwise
        """
        try:
            # Resolve to absolute path and check for path traversal
            resolved = Path(path).resolve()
            
            # Path must exist to be considered safe
            if not resolved.exists():
                logger.warning(f"Path does not exist: {path}")
                return False
            
            # Check directory requirement if specified
            if must_be_dir and not resolved.is_dir():
                logger.warning(f"Path is not a directory: {path}")
                return False
            
            # Prevent access to system directories using blacklist
            # Note: This is a defense-in-depth measure. Consider using
            # a whitelist approach for production systems.
            dangerous_paths = [
                Path('/etc'),
                Path('/sys'),
                Path('/proc'),
                Path('/dev'),
                Path('/root'),
                Path('/boot'),
                Path('/var/log'),
                Path('/usr/bin'),
                Path('/usr/sbin'),
                Path('/sbin'),
                Path('/bin'),
            ]
            
            for dangerous in dangerous_paths:
                try:
                    if resolved.is_relative_to(dangerous):
                        logger.warning(f"Blocked access to dangerous path: {resolved}")
                        return False
                except ValueError:
                    # is_relative_to raises ValueError if paths are on different drives (Windows)
                    continue
            
            # Additional check: ensure path is under user's home or common safe locations
            home = Path.home()
            safe_base_paths = [
                home,
                Path('/opt'),
                Path('/usr/local'),
                Path('/var/www'),
                Path('/tmp'),  # Allowed for temporary working directories
            ]
            
            # Check if path is under any safe base path
            is_under_safe_path = False
            for safe_path in safe_base_paths:
                try:
                    if resolved.is_relative_to(safe_path):
                        is_under_safe_path = True
                        break
                except ValueError:
                    continue
            
            if not is_under_safe_path:
                logger.warning(f"Path not under any safe base path: {resolved}")
                return False
            
            return True
        except (ValueError, OSError, RuntimeError) as e:
            logger.error(f"Path validation error for {path}: {e}")
            return False
    
    def add_context_dir(self, path: str) -> bool:
        """Add a context directory with security validation."""
        if len(self.context_dirs) >= MAX_CONTEXT_DIRS:
            logger.warning(f"Maximum number of context directories ({MAX_CONTEXT_DIRS}) reached")
            return False
        
        expanded = os.path.expanduser(path)
        
        # Security check: validate path
        if not self._is_safe_path(expanded):
            logger.warning(f"Rejected unsafe path: {path}")
            return False
        
        if os.path.isdir(expanded) and expanded not in self.context_dirs:
            self.context_dirs.append(expanded)
            logger.info(f"Added context directory: {expanded}")
            return True
        
        logger.warning(f"Failed to add context directory: {path}")
        return False
    
    def remove_context_dir(self, path: str) -> bool:
        """Remove a context directory."""
        expanded = os.path.expanduser(path)
        if expanded in self.context_dirs:
            self.context_dirs.remove(expanded)
            logger.info(f"Removed context directory: {expanded}")
            return True
        return False
    
    def list_context_dirs(self) -> List[str]:
        """List all context directories."""
        return self.context_dirs.copy()
    
    def search_context(self, query: str) -> str:
        """
        Search context directories for query using safe file reading.
        
        Security measures:
        - Validates file paths to prevent traversal
        - Limits file size to prevent memory exhaustion
        - Limits search results to prevent DoS
        - Skips binary and dangerous file types
        
        Args:
            query: The search term
            
        Returns:
            Search results as formatted string
        """
        if not self.context_dirs:
            return "No context directories configured. Use /context/add to add directories."
        
        # Validate query length
        if len(query) > 1000:
            return "Search query too long (max 1000 characters)"
        
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
        files_processed = 0
        
        logger.info(f"Searching for '{query}' in {len(self.context_dirs)} directories")
        
        for directory in self.context_dirs:
            if not os.path.isdir(directory):
                logger.warning(f"Skipping non-existent directory: {directory}")
                continue
            
            # Security check: validate directory
            if not self._is_safe_path(directory):
                logger.warning(f"Skipping unsafe directory: {directory}")
                continue
            
            # Simple file search - walk directory with depth limit
            try:
                for root, dirs, files in os.walk(directory):
                    # Calculate depth relative to starting directory
                    depth = root[len(directory):].count(os.sep)
                    if depth >= MAX_DIRECTORY_DEPTH:
                        dirs[:] = []  # Don't recurse deeper
                        continue
                    
                    # Skip hidden directories
                    dirs[:] = [d for d in dirs if not d.startswith('.')]
                    
                    for file in files:
                        # Skip hidden files and binary files by extension first (performance)
                        if file.startswith('.') or file.endswith(excluded_extensions):
                            continue
                        
                        filepath = os.path.join(root, file)
                        
                        # Check file size before validating path (performance optimization)
                        try:
                            file_size = os.path.getsize(filepath)
                            if file_size > MAX_FILE_SIZE:
                                logger.debug(f"Skipping large file: {filepath} ({file_size} bytes)")
                                continue
                        except OSError:
                            continue
                        
                        # Security check: validate file path (less frequent now)
                        if not self._is_safe_path(filepath, must_be_dir=False):
                            logger.warning(f"Skipping unsafe file path: {filepath}")
                            continue
                        
                        file_match_counts[filepath] = 0
                        
                        try:
                            # Safe file reading with proper encoding error handling
                            # Using 'replace' instead of 'ignore' to maintain data integrity
                            with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
                                for line_num, line in enumerate(f, 1):
                                    # Limit line length to prevent memory issues
                                    if len(line) > 1000:
                                        continue
                                    
                                    if query.lower() in line.lower():
                                        # Truncate long lines for display
                                        display_line = line.strip()
                                        if len(display_line) > 200:
                                            display_line = display_line[:200] + "..."
                                        
                                        results.append(f"{filepath}:{line_num}: {display_line}")
                                        file_match_counts[filepath] += 1
                                        
                                        # Limit results per file
                                        if file_match_counts[filepath] >= MAX_RESULTS_PER_FILE:
                                            break
                        except (IOError, OSError, UnicodeDecodeError) as e:
                            logger.debug(f"Error reading file {filepath}: {e}")
                            continue
                        
                        files_processed += 1
                        
                        # Limit total files processed
                        if files_processed >= MAX_FILES_TO_PROCESS:
                            logger.info(f"Reached max files to process ({MAX_FILES_TO_PROCESS})")
                            break
                        
                        # Limit total results
                        if len(results) >= MAX_SEARCH_RESULTS:
                            break
                    
                    if len(results) >= MAX_SEARCH_RESULTS:
                        break
            except (IOError, OSError) as e:
                error_msg = f"Error searching {directory}: {e}"
                logger.error(error_msg)
                results.append(error_msg)
        
        if not results:
            msg = f"No matches found for '{query}' in {len(self.context_dirs)} context directories."
            logger.info(msg)
            return msg
        
        logger.info(f"Found {len(results)} results for '{query}'")
        return "\n".join(results[:MAX_SEARCH_RESULTS])  # Return top results


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
    
    def log_message(self, format, *args):
        """Override to provide cleaner logging."""
        logger.info(format % args)
    
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
        """Read and parse JSON body with size limits."""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length == 0:
            return {}
        
        # Prevent large payload attacks
        if content_length > MAX_REQUEST_SIZE:
            raise ValueError(f"Request body too large: {content_length} bytes (max {MAX_REQUEST_SIZE})")
        
        body = self.rfile.read(content_length)
        
        try:
            return json.loads(body.decode('utf-8'))
        except UnicodeDecodeError as e:
            logger.error(f"Invalid UTF-8 in request body: {e}")
            raise ValueError("Request body must be valid UTF-8")
        except json.JSONDecodeError as e:
            logger.error(f"Invalid JSON in request body: {e}")
            raise ValueError(f"Invalid JSON: {e}")
    
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
        """Handle POST requests with input validation."""
        try:
            data = self._read_json_body()
        except json.JSONDecodeError as e:
            logger.warning(f"JSON decode error: {e}")
            self._send_error_response("Invalid JSON")
            return
        except ValueError as e:
            logger.warning(f"Invalid request body: {e}")
            self._send_error_response(str(e))
            return
        
        if self.path == '/chat/send':
            conv_id = data.get('conversation_id', 'default')
            message = data.get('message', '')
            context = data.get('context', '')
            
            # Validate conversation ID
            if not isinstance(conv_id, str) or len(conv_id) > 100:
                self._send_error_response("Invalid conversation_id")
                return
            
            # Validate message
            if not message or not isinstance(message, str):
                self._send_error_response("Message required and must be a string")
                return
            
            if len(message) > MAX_MESSAGE_LENGTH:
                self._send_error_response(f"Message exceeds maximum length of {MAX_MESSAGE_LENGTH}")
                return
            
            try:
                response = self.chat_manager.send_to_llm(conv_id, message, context)
                self._send_json_response({
                    "response": response,
                    "conversation_id": conv_id
                })
            except ValueError as e:
                logger.error(f"Error in chat send: {e}")
                self._send_error_response(str(e))
        
        elif self.path == '/chat/clear':
            conv_id = data.get('conversation_id', 'default')
            
            # Validate conversation ID
            if not isinstance(conv_id, str) or len(conv_id) > 100:
                self._send_error_response("Invalid conversation_id")
                return
            
            success = self.chat_manager.clear_conversation(conv_id)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/add':
            path = data.get('path', '')
            
            # Validate path
            if not path or not isinstance(path, str):
                self._send_error_response("Path required and must be a string")
                return
            
            if len(path) > 1000:
                self._send_error_response("Path too long (max 1000 characters)")
                return
            
            success = self.context_manager.add_context_dir(path)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/remove':
            path = data.get('path', '')
            
            # Validate path
            if not path or not isinstance(path, str):
                self._send_error_response("Path required and must be a string")
                return
            
            success = self.context_manager.remove_context_dir(path)
            self._send_json_response({"success": success})
        
        elif self.path == '/context/search':
            query = data.get('query', '')
            
            # Validate query
            if not query or not isinstance(query, str):
                self._send_error_response("Query required and must be a string")
                return
            
            if len(query) > 1000:
                self._send_error_response("Query too long (max 1000 characters)")
                return
            
            try:
                results = self.context_manager.search_context(query)
                self._send_json_response({"results": results})
            except (OSError, IOError, ValueError) as e:
                logger.error(f"Error in context search: {e}")
                self._send_error_response("Search failed")
        
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
    # Configure logging for the server
    logging.basicConfig(
        level=logging.INFO,
        format='[%(asctime)s] %(levelname)s: %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S'
    )
    
    server = HTTPServer((host, port), IDERequestHandler)
    logger.info(f"IDE Server starting on http://{host}:{port}")
    logger.info(f"Chat endpoint: http://{host}:{port}/chat/send")
    logger.info(f"Health check: http://{host}:{port}/health")
    logger.info("Press Ctrl+C to stop")
    
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        logger.info("Shutting down server...")
        server.shutdown()


if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='IDE Server for Emacs')
    parser.add_argument('--host', default='127.0.0.1', help='Host to bind to')
    parser.add_argument('--port', type=int, default=9999, help='Port to bind to')
    
    args = parser.parse_args()
    run_server(args.host, args.port)
