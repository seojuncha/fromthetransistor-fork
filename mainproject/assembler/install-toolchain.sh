#!/bin/bash

# 설정
TOOLCHAIN_URL="https://developer.arm.com/-/media/Files/downloads/gnu/14.2.rel1/binrel/arm-gnu-toolchain-14.2.rel1-x86_64-arm-none-eabi.tar.xz"
INSTALL_DIR="/opt/arm-gnu-toolchain-14.2.rel1"
PROFILE_FILE="$HOME/.bashrc"
TEMP_DIR="/tmp/arm-toolchain"

# 1. 필요한 디렉토리 생성
echo "[*] Creating temporary directory at $TEMP_DIR..."
mkdir -p "$TEMP_DIR"
cd "$TEMP_DIR" || exit 1

# 2. 다운로드
echo "[*] Downloading toolchain from:"
echo "    $TOOLCHAIN_URL"
wget -O toolchain.tar.xz "$TOOLCHAIN_URL" || { echo "Download failed"; exit 1; }

# 3. 압축 해제
echo "[*] Extracting to $INSTALL_DIR..."
sudo mkdir -p "$INSTALL_DIR"
sudo tar -xf toolchain.tar.xz -C "$INSTALL_DIR" --strip-components=1

# 4. 환경변수 등록
if ! grep -q "$INSTALL_DIR/bin" "$PROFILE_FILE"; then
  echo "[*] Adding toolchain path to $PROFILE_FILE..."
  echo "# ARM GNU Toolchain" >> "$PROFILE_FILE"
  echo "export PATH=\"$INSTALL_DIR/bin:\$PATH\"" >> "$PROFILE_FILE"
else
  echo "[*] Toolchain path already in $PROFILE_FILE"
fi

# 5. 완료
echo "[*] Done. Run the following command or restart your terminal:"
echo "    source $PROFILE_FILE"
echo "[*] Then verify installation with:"
echo "    arm-none-eabi-gcc --version"