#!/bin/bash

# Release script for multi-egg mono-repo
# Usage: ./release.sh "schematra:1.2.3,oauthtoothy:2.1.0"
# Usage: ./release.sh "schematra:1.2.3"

if [ -z "$1" ]; then
    echo "Usage: $0 'egg1:version1,egg2:version2,...'"
    echo "Example: $0 'schematra:1.2.3,oauthtoothy:2.1.0'"
    exit 1
fi

# Check if we're in a git repo
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Check if working directory is clean
if ! git diff-index --quiet HEAD --; then
    echo "Error: Working directory is not clean. Please commit your changes first."
    exit 1
fi

IFS=',' read -ra EGGS <<< "$1"
COMMIT=$(git rev-parse HEAD)

echo "Releasing eggs from commit: $COMMIT"
echo

for egg_version in "${EGGS[@]}"; do
    IFS=':' read -r EGG VERSION <<< "$egg_version"
    
    echo "Processing $EGG v$VERSION..."
    
    # Check if egg directory exists
    if [ ! -d "eggs/$EGG" ]; then
        echo "Error: eggs/$EGG directory does not exist"
        exit 1
    fi
    
    # Create tarball
    echo "  Creating tarball..."
    git archive --format=tar.gz --prefix=${EGG}-${VERSION}/ HEAD:eggs/${EGG} > ${EGG}-${VERSION}.tar.gz
    
    # Check if tag already exists
    if git rev-parse ${EGG}-v${VERSION} >/dev/null 2>&1; then
        echo "  Warning: Tag ${EGG}-v${VERSION} already exists, skipping..."
        continue
    fi
    
    # Tag the commit
    echo "  Creating tag ${EGG}-v${VERSION}..."
    git tag ${EGG}-v${VERSION}
    
    # Create GitHub release (if gh is available)
    if command -v gh &> /dev/null; then
        echo "  Creating GitHub release..."
        gh release create ${EGG}-v${VERSION} ${EGG}-${VERSION}.tar.gz --title "${EGG} v${VERSION}" --notes "Release ${EGG} version ${VERSION}"
    else
        echo "  GitHub CLI not available, skipping release creation"
        echo "  You can manually create a release and upload ${EGG}-${VERSION}.tar.gz"
    fi
    
    echo "  ✓ ${EGG} v${VERSION} released"
    echo
done

echo "Pushing tags to remote..."
git push origin --tags

echo "All releases completed!"