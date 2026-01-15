# Basic Examples (Level 1)

**适用人群**: 初学者、快速原型开发
**复杂度**: 1-3 行代码完成任务

## 推荐示例

| 示例 | 描述 | 文件 |
|------|------|------|
| Hello SSL | 最简单的 SSL 示例 | `../hello_ssl.pas` |
| Simple HTTPS | 简单 HTTPS 请求 | `../simple_https_demo.pas` |
| Hash Calculator | 哈希计算工具 | `../hash_calculator.pas` |
| Crypto Simple | 简单加密示例 | `../example_crypto_simple.pas` |

## 快速开始

```pascal
uses fafafa.ssl.quick;

var
  Response: string;
begin
  Response := TSSLQuick.Get('https://api.example.com/data');
  WriteLn(Response);
end;
```

## 下一步

当你需要更多控制时，请查看 [Production](../production/) 或 [Advanced](../Advanced/) 示例。
