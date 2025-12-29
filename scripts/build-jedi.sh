#!/bin/bash
set -euo pipefail

# Build Reliable Jedi Container
# Usage: ./build-jedi.sh [--force] [--no-cache]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DOCKER_DIR="$PROJECT_ROOT/docker"

FORCE_BUILD=false
NO_CACHE=false
IMAGE_NAME="reliable-jedi"
IMAGE_TAG="latest"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --force)
            FORCE_BUILD=true
            shift
            ;;
        --no-cache)
            NO_CACHE=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--force] [--no-cache]"
            echo "  --force    Force rebuild even if image exists"
            echo "  --no-cache Use --no-cache for docker build"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "🐳 Building Reliable Jedi Container"
echo "=================================="

# Check if Docker is available
if ! command -v docker &> /dev/null; then
    echo "❌ Docker is not installed or not in PATH"
    exit 1
fi

# Check if image already exists
if docker image inspect "$IMAGE_NAME:$IMAGE_TAG" &> /dev/null; then
    if [ "$FORCE_BUILD" = false ]; then
        echo "✅ Image $IMAGE_NAME:$IMAGE_TAG already exists"
        echo "   Use --force to rebuild"
        exit 0
    else
        echo "🔄 Force rebuilding existing image"
    fi
fi

# Build Docker image
echo "📦 Building Docker image..."
cd "$DOCKER_DIR"

BUILD_ARGS=()
if [ "$NO_CACHE" = true ]; then
    BUILD_ARGS+=(--no-cache)
fi

if docker build "${BUILD_ARGS[@]}" -t "$IMAGE_NAME:$IMAGE_TAG" .; then
    echo "✅ Successfully built $IMAGE_NAME:$IMAGE_TAG"
else
    echo "❌ Failed to build Docker image"
    echo ""
    echo "💡 Troubleshooting tips:"
    echo "   1. Try clearing Docker cache: docker builder prune -af"
    echo "   2. Rebuild with --no-cache flag: $0 --no-cache"
    echo "   3. Check Docker logs above for specific errors"
    echo ""
    exit 1
fi

# Extract deployment archive
echo "📤 Extracting deployment files..."
TEMP_CONTAINER=$(docker create "$IMAGE_NAME:$IMAGE_TAG")
mkdir -p "$PROJECT_ROOT/dist"

if docker cp "$TEMP_CONTAINER:/opt/jedi-deployment.tar.gz" "$PROJECT_ROOT/dist/"; then
    echo "✅ Deployment archive extracted to dist/"
else
    echo "❌ Failed to extract deployment archive"
    docker rm "$TEMP_CONTAINER" &> /dev/null || true
    exit 1
fi

docker rm "$TEMP_CONTAINER" &> /dev/null || true

# Test the build
echo "🧪 Testing the build..."
if docker run --rm "$IMAGE_NAME:$IMAGE_TAG" python -c "import jedi; print('Jedi version:', jedi.__version__)"; then
    echo "✅ Build test passed"
else
    echo "❌ Build test failed"
    exit 1
fi

echo ""
echo "🎉 Build completed successfully!"
echo "   Image: $IMAGE_NAME:$IMAGE_TAG"
echo "   Deployment archive: $PROJECT_ROOT/dist/jedi-deployment.tar.gz"
echo ""
echo "Next steps:"
echo "   ./scripts/deploy-jedi.sh [venv_path]"