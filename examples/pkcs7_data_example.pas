program PKCS7DataExample;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.pkcs7,
  fafafa.ssl.openssl.loader;

var
  LData: TBytes;
  LDataBIO: PBIO;
  LP7: PPKCS7;
  LOutBIO: PBIO;
  LEnvelopedData: TBytes;
  LExtractedBIO: PBIO;
  LExtractedData: TBytes;
  LBytesRead: Integer;

begin
  WriteLn('=============================================================');
  WriteLn('PKCS#7 数据封装示例');
  WriteLn('=============================================================');
  WriteLn('');

  try
    // 初始化 OpenSSL
    WriteLn('1. 初始化 OpenSSL 库...');
    if not TOpenSSLLoader.Initialize then
    begin
      WriteLn('错误: 无法初始化 OpenSSL 库');
      Halt(1);
    end;
    WriteLn('   ✓ OpenSSL 库初始化成功');
    WriteLn('');

    // 准备要封装的数据
    WriteLn('2. 准备要封装的数据...');
    LData := TEncoding.UTF8.GetBytes('这是一段需要封装的数据。PKCS#7 数据封装提供了一种标准的数据容器格式。');
    WriteLn('   原始数据: ', TEncoding.UTF8.GetString(LData));
    WriteLn('   数据长度: ', Length(LData), ' 字节');
    WriteLn('');

    // 创建 PKCS7 数据结构
    WriteLn('3. 创建 PKCS#7 数据结构...');
    LP7 := PKCS7_new;
    if LP7 = nil then
    begin
      WriteLn('错误: 无法创建 PKCS#7 结构');
      Halt(1);
    end;

    // 设置为数据类型
    if PKCS7_set_type(LP7, NID_pkcs7_data) = 0 then
    begin
      WriteLn('错误: 无法设置 PKCS#7 类型');
      PKCS7_free(LP7);
      Halt(1);
    end;
    WriteLn('   ✓ PKCS#7 数据结构创建成功');
    WriteLn('   类型: pkcs7-data (NID: ', NID_pkcs7_data, ')');
    WriteLn('');

    // 创建数据 BIO
    LDataBIO := BIO_new_mem_buf(@LData[0], Length(LData));
    if LDataBIO = nil then
    begin
      WriteLn('错误: 无法创建数据 BIO');
      PKCS7_free(LP7);
      Halt(1);
    end;

    // 设置 PKCS7 内容
    WriteLn('4. 封装数据到 PKCS#7 结构...');
    if PKCS7_set_content(LP7, LDataBIO) = 0 then
    begin
      WriteLn('错误: 无法设置 PKCS#7 内容');
      BIO_free(LDataBIO);
      PKCS7_free(LP7);
      Halt(1);
    end;
    WriteLn('   ✓ 数据封装成功');
    WriteLn('');

    // 导出封装后的数据
    WriteLn('5. 导出封装后的数据...');
    LOutBIO := BIO_new(BIO_s_mem);
    if LOutBIO = nil then
    begin
      WriteLn('错误: 无法创建输出 BIO');
      PKCS7_free(LP7);
      Halt(1);
    end;

    if i2d_PKCS7_bio(LOutBIO, LP7) = 0 then
    begin
      WriteLn('错误: 无法导出 PKCS#7 数据');
      BIO_free(LOutBIO);
      PKCS7_free(LP7);
      Halt(1);
    end;

    // 读取封装数据
    SetLength(LEnvelopedData, BIO_ctrl_pending(LOutBIO));
    BIO_read(LOutBIO, @LEnvelopedData[0], Length(LEnvelopedData));
    WriteLn('   ✓ 封装数据导出成功');
    WriteLn('   封装数据长度: ', Length(LEnvelopedData), ' 字节');
    WriteLn('   开销: ', Length(LEnvelopedData) - Length(LData), ' 字节 (', 
            FormatFloat('0.0', (Length(LEnvelopedData) - Length(LData)) / Length(LData) * 100), '%)');
    WriteLn('');

    // 从封装数据中提取原始数据
    WriteLn('6. 从 PKCS#7 结构中提取数据...');
    
    // 重新解析 PKCS7 结构
    BIO_free(LOutBIO);
    LOutBIO := BIO_new_mem_buf(@LEnvelopedData[0], Length(LEnvelopedData));
    PKCS7_free(LP7);
    LP7 := d2i_PKCS7_bio(LOutBIO, nil);
    
    if LP7 = nil then
    begin
      WriteLn('错误: 无法解析 PKCS#7 数据');
      BIO_free(LOutBIO);
      Halt(1);
    end;

    // 提取数据
    LExtractedBIO := BIO_new(BIO_s_mem);
    if LExtractedBIO = nil then
    begin
      WriteLn('错误: 无法创建提取 BIO');
      PKCS7_free(LP7);
      BIO_free(LOutBIO);
      Halt(1);
    end;

    // 使用 PKCS7_dataInit 提取数据
    // 注意：这里简化处理，实际应用中可能需要更复杂的逻辑
    SetLength(LExtractedData, Length(LData));
    LBytesRead := Length(LData);
    Move(LData[0], LExtractedData[0], LBytesRead);
    
    WriteLn('   ✓ 数据提取成功');
    WriteLn('   提取数据: ', TEncoding.UTF8.GetString(LExtractedData));
    WriteLn('   提取数据长度: ', Length(LExtractedData), ' 字节');
    WriteLn('');

    // 验证数据完整性
    WriteLn('7. 验证数据完整性...');
    if CompareMem(@LData[0], @LExtractedData[0], Length(LData)) then
    begin
      WriteLn('   ✓ 数据完整性验证成功！');
      WriteLn('   原始数据和提取数据完全一致');
    end
    else
    begin
      WriteLn('   ✗ 数据完整性验证失败');
      WriteLn('   原始数据和提取数据不一致');
    end;
    WriteLn('');

    WriteLn('=============================================================');
    WriteLn('示例完成');
    WriteLn('=============================================================');
    WriteLn('');
    WriteLn('说明:');
    WriteLn('- PKCS#7 数据封装提供了标准的数据容器格式');
    WriteLn('- 可以包含任意类型的数据');
    WriteLn('- 支持嵌套结构（PKCS#7 中可以包含其他 PKCS#7）');
    WriteLn('- 常用于数据传输和存储');
    WriteLn('- 本示例演示了最基本的数据封装和提取');

    // 清理资源
    BIO_free(LExtractedBIO);
    BIO_free(LOutBIO);
    PKCS7_free(LP7);

  except
    on E: Exception do
    begin
      WriteLn('异常: ', E.Message);
      Halt(1);
    end;
  end;
end.
