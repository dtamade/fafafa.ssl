#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() { echo "[FAIL] $1"; exit 1; }

mkdir -p tmp/runtime_contracts/tools

# 03_file_encryption roundtrip
printf 'hello tool example\n' > tmp/runtime_contracts/tools/plain1.txt
fpc -Fu./src examples/03_file_encryption.pas -otmp/tools_file_encryption >/tmp/tools_file_encryption.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encryption.log; fail '03_file_encryption should compile'; }
./tmp/tools_file_encryption encrypt tmp/runtime_contracts/tools/plain1.txt tmp/runtime_contracts/tools/plain1.enc secret123 >/tmp/tools_file_encryption_run1.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encryption_run1.log; fail '03_file_encryption encrypt should run'; }
./tmp/tools_file_encryption decrypt tmp/runtime_contracts/tools/plain1.enc tmp/runtime_contracts/tools/plain1.out secret123 >/tmp/tools_file_encryption_run2.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encryption_run2.log; fail '03_file_encryption decrypt should run'; }
diff -q tmp/runtime_contracts/tools/plain1.txt tmp/runtime_contracts/tools/plain1.out >/dev/null || fail '03_file_encryption roundtrip mismatch'
rg -F --quiet -- '[PASS] file encryption example completed' /tmp/tools_file_encryption_run2.log || fail '03_file_encryption missing completion marker'

# file_encrypt roundtrip
printf 'hello file_encrypt\n' > tmp/runtime_contracts/tools/plain2.txt
fpc -Fu./src examples/file_encrypt/file_encrypt.pas -otmp/tools_file_encrypt >/tmp/tools_file_encrypt.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encrypt.log; fail 'file_encrypt should compile'; }
./tmp/tools_file_encrypt -e tmp/runtime_contracts/tools/plain2.txt tmp/runtime_contracts/tools/plain2.enc secret123 >/tmp/tools_file_encrypt_run1.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encrypt_run1.log; fail 'file_encrypt encrypt should run'; }
./tmp/tools_file_encrypt -d tmp/runtime_contracts/tools/plain2.enc tmp/runtime_contracts/tools/plain2.out secret123 >/tmp/tools_file_encrypt_run2.log 2>&1 || { sed -n '1,200p' /tmp/tools_file_encrypt_run2.log; fail 'file_encrypt decrypt should run'; }
diff -q tmp/runtime_contracts/tools/plain2.txt tmp/runtime_contracts/tools/plain2.out >/dev/null || fail 'file_encrypt roundtrip mismatch'

# password_hash roundtrip
fpc -Fu./src examples/password_hash/password_hash.pas -otmp/tools_password_hash >/tmp/tools_password_hash.log 2>&1 || { sed -n '1,200p' /tmp/tools_password_hash.log; fail 'password_hash should compile'; }
./tmp/tools_password_hash -hash secret123 1000 >/tmp/tools_password_hash_hash.log 2>&1 || { sed -n '1,200p' /tmp/tools_password_hash_hash.log; fail 'password_hash hash should run'; }
HASH_LINE=$(rg '^pbkdf2:' /tmp/tools_password_hash_hash.log | head -n 1)
test -n "$HASH_LINE" || fail 'password_hash should emit hash line'
./tmp/tools_password_hash -verify secret123 "$HASH_LINE" >/tmp/tools_password_hash_verify.log 2>&1 || true
rg -F --quiet -- '[OK] [OK] [OK] Password verification SUCCESSFUL!' /tmp/tools_password_hash_verify.log || fail 'password_hash verify should succeed'

# digital_signature roundtrip
printf 'sign me\n' > tmp/runtime_contracts/tools/plain3.txt
fpc -Fu./src examples/digital_signature/digital_signature.pas -otmp/tools_digital_signature >/tmp/tools_digital_signature.log 2>&1 || { sed -n '1,200p' /tmp/tools_digital_signature.log; fail 'digital_signature should compile'; }
./tmp/tools_digital_signature -g tmp/runtime_contracts/tools/private.pem tmp/runtime_contracts/tools/public.pem 2048 >/tmp/tools_digital_signature_gen.log 2>&1 || { sed -n '1,200p' /tmp/tools_digital_signature_gen.log; fail 'digital_signature generate should run'; }
./tmp/tools_digital_signature -s tmp/runtime_contracts/tools/plain3.txt tmp/runtime_contracts/tools/plain3.sig tmp/runtime_contracts/tools/private.pem >/tmp/tools_digital_signature_sign.log 2>&1 || { sed -n '1,200p' /tmp/tools_digital_signature_sign.log; fail 'digital_signature sign should run'; }
./tmp/tools_digital_signature -v tmp/runtime_contracts/tools/plain3.txt tmp/runtime_contracts/tools/plain3.sig tmp/runtime_contracts/tools/public.pem >/tmp/tools_digital_signature_verify.log 2>&1 || { sed -n '1,200p' /tmp/tools_digital_signature_verify.log; fail 'digital_signature verify should run'; }

# hmac_tool generate + verify
printf 'hmac me\n' > tmp/runtime_contracts/tools/plain4.txt
printf 'supersecretkey' > tmp/runtime_contracts/tools/key.txt
fpc -Fu./src examples/hmac_tool/hmac_tool.lpr -otmp/tools_hmac_tool >/tmp/tools_hmac_tool.log 2>&1 || { sed -n '1,220p' /tmp/tools_hmac_tool.log; fail 'hmac_tool should compile'; }
./tmp/tools_hmac_tool generate -i tmp/runtime_contracts/tools/plain4.txt -o tmp/runtime_contracts/tools/plain4.hmac -k @tmp/runtime_contracts/tools/key.txt -a sha256 --hex >/tmp/tools_hmac_tool_gen.log 2>&1 || { sed -n '1,220p' /tmp/tools_hmac_tool_gen.log; fail 'hmac_tool generate should run'; }
./tmp/tools_hmac_tool verify -i tmp/runtime_contracts/tools/plain4.txt -m tmp/runtime_contracts/tools/plain4.hmac -k @tmp/runtime_contracts/tools/key.txt -a sha256 --hex >/tmp/tools_hmac_tool_verify.log 2>&1 || { sed -n '1,220p' /tmp/tools_hmac_tool_verify.log; fail 'hmac_tool verify should run'; }
rg -F --quiet -- 'HMAC verification: SUCCESS' /tmp/tools_hmac_tool_verify.log || fail 'hmac_tool verify should report success'

echo '[PASS] tool example programs stay green at runtime'
