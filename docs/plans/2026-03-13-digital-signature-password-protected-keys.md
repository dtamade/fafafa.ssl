# Digital Signature Example: Password-Protected Private Keys

## Goal
- 为 `examples/digital_signature/digital_signature.pas` 增加 **私钥密码保护**能力：
  - 生成密钥对时可加密输出私钥 PEM（AES-256-CBC）。
  - 签名时可读取带密码的私钥 PEM。
- 保持默认行为不变：不提供密码时仍生成/读取未加密私钥。

## Non-Goals
- 不实现时间戳签名（需要网络/上层传输框架）。
- 不实现批量签名（本批次仅完成密码保护闭环）。

## Architecture
- 复用 `fafafa.ssl.openssl.api.pem` 的辅助函数（已提供密码回调）：
  - `SavePrivateKeyToPEM(File, PKey, Password)`：当 `Password<>''` 时使用 `EVP_aes_256_cbc()` 加密写出。
  - `LoadPrivateKeyFromPEM(File, Password)`：读取带密码的私钥 PEM。
- CLI 参数（向后兼容）：
  - `-p <password>` / `--password <password>`：
    - `-g`：加密输出私钥
    - `-s`：读取带密码的私钥

## Files
- Modify: `examples/digital_signature/digital_signature.pas`
- Modify: `examples/digital_signature/README.md`
- Add: `tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh`

## Step-by-step
1) 实现参数解析：识别 `-p/--password`（不改变既有位置参数语义）
2) 生成密钥对：
   - 替换为调用 `SavePrivateKeyToPEM(..., Password)` 输出私钥
3) 签名：
   - 替换为调用 `LoadPrivateKeyFromPEM(..., Password)` 读取私钥
4) 文档同步：
   - README 增加密码保护用法示例
5) 添加 contract test（避免编译产物污染工作区）：
   - `bash -n tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh`
   - `bash tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh`

## Expected Outputs / Acceptance
- `-p/--password` 存在时：
  - 生成的私钥 PEM 为加密格式（包含 `ENCRYPTED` 标记，AES-256-CBC）。
  - 签名/验签流程成功。
  - 使用错误密码签名应失败。
- 不带 `-p/--password` 时：
  - 行为与此前一致（生成未加密私钥，签名/验签不受影响）。
