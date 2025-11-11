unit fafafa.ssl.http.simple;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ fafafa.ssl 简化HTTP客户端
  
  目标：让HTTPS请求只需一行代码
  
  示例：
    LResponse := TSimpleHTTPSClient.Get('https://api.example.com/data');
    LResponse := TSimpleHTTPSClient.Post('https://api.example.com/data', '{"key":"value"}');
}

interface

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl, fafafa.ssl.base;

const
  DEFAULT_TIMEOUT = 30000;        // 30秒
  DEFAULT_MAX_REDIRECTS = 5;
  DEFAULT_BUFFER_SIZE = 16384;    // 16KB
  
  USER_AGENT = 'fafafa.ssl-simple/1.0';

type
  { HTTP方法 }
  THTTPMethod = (
    httpGET,
    httpPOST,
    httpPUT,
    httpDELETE,
    httpHEAD,
    httpPATCH,
    httpOPTIONS
  );

  { 进度回调 }
  TProgressCallback = procedure(ATotal, ACurrent: Int64) of object;

  { HTTPS选项 }
  THTTPSOptions = record
    Timeout: Integer;           // 超时（毫秒）
    FollowRedirects: Boolean;   // 跟随重定向
    MaxRedirects: Integer;      // 最大重定向次数
    VerifyPeer: Boolean;        // 验证服务器证书
    ClientCert: string;         // 客户端证书（PEM）
    ClientKey: string;          // 客户端私钥（PEM）
    CAFile: string;             // CA证书文件
    UserAgent: string;          // User-Agent
    Headers: TStringList;       // 自定义请求头
  end;

  { HTTP响应 }
  THTTPResponse = record
    StatusCode: Integer;        // HTTP状态码
    StatusText: string;         // 状态文本
    Headers: TStringList;       // 响应头
    Body: string;               // 响应体
    ContentLength: Int64;       // 内容长度
    Success: Boolean;           // 是否成功
    ErrorMessage: string;       // 错误信息
  end;

  { 简化HTTPS客户端 }
  TSimpleHTTPSClient = class
  private
    class function ParseURL(const AURL: string; 
      out AProtocol, AHost, APath: string; 
      out APort: Word): Boolean;
    class function BuildRequest(AMethod: THTTPMethod; 
      const AHost, APath, ABody: string;
      AHeaders: TStringList): RawByteString;
    class function ParseResponse(const ARawResponse: string): THTTPResponse;
    class function DoRequest(AMethod: THTTPMethod;
      const AURL, ABody: string;
      const AOptions: THTTPSOptions): THTTPResponse;
  public
    { 创建默认选项 }
    class function DefaultOptions: THTTPSOptions;
    
    { 简化方法 - 使用默认选项 }
    class function Get(const AURL: string): string;
    class function Post(const AURL: string; const AData: string): string;
    
    { 完整方法 - 自定义选项 }
    class function GetEx(const AURL: string; 
      const AOptions: THTTPSOptions): THTTPResponse;
    class function PostEx(const AURL, AData: string;
      const AOptions: THTTPSOptions): THTTPResponse;
    class function PutEx(const AURL, AData: string;
      const AOptions: THTTPSOptions): THTTPResponse;
    class function DeleteEx(const AURL: string;
      const AOptions: THTTPSOptions): THTTPResponse;
    
    { 文件操作 }
    class function Download(const AURL, AFilePath: string;
      AProgressCallback: TProgressCallback = nil): Boolean;
    class function Upload(const AURL, AFilePath: string): Boolean;
  end;

  { HTTPS客户端异常 }
  EHTTPSClientException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const AMsg: string; AStatusCode: Integer = 0);
    property StatusCode: Integer read FStatusCode;
  end;

implementation

{ EHTTPSClientException }

constructor EHTTPSClientException.Create(const AMsg: string; AStatusCode: Integer);
begin
  inherited Create(AMsg);
  FStatusCode := AStatusCode;
end;

{ TSimpleHTTPSClient }

class function TSimpleHTTPSClient.DefaultOptions: THTTPSOptions;
begin
  Result.Timeout := DEFAULT_TIMEOUT;
  Result.FollowRedirects := True;
  Result.MaxRedirects := DEFAULT_MAX_REDIRECTS;
  Result.VerifyPeer := True;
  Result.ClientCert := '';
  Result.ClientKey := '';
  Result.CAFile := '';
  Result.UserAgent := USER_AGENT;
  Result.Headers := TStringList.Create;
end;

class function TSimpleHTTPSClient.ParseURL(const AURL: string; 
  out AProtocol, AHost, APath: string; 
  out APort: Word): Boolean;
var
  LPos, LPortPos: Integer;
  LTemp: string;
begin
  Result := False;
  AProtocol := 'https';
  APort := 443;
  
  LTemp := AURL;
  
  // 解析协议
  if Pos('https://', LowerCase(LTemp)) = 1 then
  begin
    Delete(LTemp, 1, 8);
    AProtocol := 'https';
    APort := 443;
  end
  else if Pos('http://', LowerCase(LTemp)) = 1 then
  begin
    Delete(LTemp, 1, 7);
    AProtocol := 'http';
    APort := 80;
  end;
  
  // 查找路径
  LPos := Pos('/', LTemp);
  if LPos > 0 then
  begin
    AHost := Copy(LTemp, 1, LPos - 1);
    APath := Copy(LTemp, LPos, Length(LTemp));
  end
  else
  begin
    AHost := LTemp;
    APath := '/';
  end;
  
  // 解析端口
  LPortPos := Pos(':', AHost);
  if LPortPos > 0 then
  begin
    try
      APort := StrToInt(Copy(AHost, LPortPos + 1, Length(AHost)));
      AHost := Copy(AHost, 1, LPortPos - 1);
    except
      Exit;
    end;
  end;
  
  Result := (AHost <> '');
end;

class function TSimpleHTTPSClient.BuildRequest(AMethod: THTTPMethod; 
  const AHost, APath, ABody: string;
  AHeaders: TStringList): RawByteString;
const
  METHOD_NAMES: array[THTTPMethod] of string = (
    'GET', 'POST', 'PUT', 'DELETE', 'HEAD', 'PATCH', 'OPTIONS'
  );
var
  i: Integer;
  LHasContentType: Boolean;
begin
  // 请求行
  Result := METHOD_NAMES[AMethod] + ' ' + APath + ' HTTP/1.1' + #13#10;
  
  // 必需头部
  Result := Result + 'Host: ' + AHost + #13#10;
  Result := Result + 'Connection: close' + #13#10;
  
  // 自定义头部
  LHasContentType := False;
  if Assigned(AHeaders) then
  begin
    for i := 0 to AHeaders.Count - 1 do
    begin
      Result := Result + AHeaders[i] + #13#10;
      if Pos('Content-Type:', AHeaders[i]) = 1 then
        LHasContentType := True;
    end;
  end;
  
  // 请求体
  if ABody <> '' then
  begin
    if not LHasContentType then
      Result := Result + 'Content-Type: application/octet-stream' + #13#10;
    Result := Result + 'Content-Length: ' + IntToStr(Length(ABody)) + #13#10;
  end;
  
  // 空行
  Result := Result + #13#10;
  
  // 添加请求体
  if ABody <> '' then
    Result := Result + ABody;
end;

class function TSimpleHTTPSClient.ParseResponse(const ARawResponse: string): THTTPResponse;
var
  LLines: TStringList;
  LLine: string;
  LPos, LBodyStart: Integer;
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Headers := TStringList.Create;
  Result.Success := False;
  
  if ARawResponse = '' then
  begin
    Result.ErrorMessage := '空响应';
    Exit;
  end;
  
  // 查找响应体起始位置
  LBodyStart := Pos(#13#10#13#10, ARawResponse);
  
  LLines := TStringList.Create;
  try
    // 解析头部
    if LBodyStart > 0 then
      LLines.Text := Copy(ARawResponse, 1, LBodyStart)
    else
      LLines.Text := ARawResponse;
    
    if LLines.Count = 0 then
    begin
      Result.ErrorMessage := '无效响应';
      Exit;
    end;
    
    // 解析状态行
    LLine := LLines[0];
    LPos := Pos(' ', LLine);
    if LPos > 0 then
    begin
      Delete(LLine, 1, LPos);
      LPos := Pos(' ', LLine);
      if LPos > 0 then
      begin
        try
          Result.StatusCode := StrToInt(Copy(LLine, 1, LPos - 1));
          Result.StatusText := Copy(LLine, LPos + 1, Length(LLine));
        except
          Result.StatusCode := 0;
        end;
      end;
    end;
    
    // 解析响应头
    for i := 1 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[i]);
      if LLine <> '' then
      begin
        Result.Headers.Add(LLine);
        
        // 提取Content-Length
        if Pos('Content-Length:', LLine) = 1 then
        begin
          try
            Result.ContentLength := StrToInt64(
              Trim(Copy(LLine, 16, Length(LLine))));
          except
            Result.ContentLength := 0;
          end;
        end;
      end;
    end;
    
    // 提取响应体
    if LBodyStart > 0 then
      Result.Body := Copy(ARawResponse, LBodyStart + 4, Length(ARawResponse));
    
    // 判断成功
    Result.Success := (Result.StatusCode >= 200) and (Result.StatusCode < 300);
    
  finally
    LLines.Free;
  end;
end;

class function TSimpleHTTPSClient.DoRequest(AMethod: THTTPMethod;
  const AURL, ABody: string;
  const AOptions: THTTPSOptions): THTTPResponse;
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LProtocol, LHost, LPath: string;
  LPort: Word;
  LRequest: RawByteString;
  LBuffer: array[0..DEFAULT_BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
  LResponse: TMemoryStream;
  LRawResponse: string;
  LStartTime: TDateTime;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Headers := TStringList.Create;
  Result.Success := False;
  
  // 解析URL
  if not ParseURL(AURL, LProtocol, LHost, LPath, LPort) then
  begin
    Result.ErrorMessage := 'URL解析失败';
    Exit;
  end;
  
  try
    LStartTime := Now;
    
    // 创建SSL上下文
    LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
    if LContext = nil then
    begin
      Result.ErrorMessage := '创建SSL上下文失败';
      Exit;
    end;
    
    // 配置证书验证
    if AOptions.VerifyPeer then
      LContext.SetVerifyMode([sslVerifyPeer])
    else
      LContext.SetVerifyMode([sslVerifyNone]);
    
    // 加载CA证书
    if AOptions.CAFile <> '' then
      LContext.LoadCAFile(AOptions.CAFile);
    
    // 加载客户端证书
    if AOptions.ClientCert <> '' then
    begin
      LContext.LoadCertificate(AOptions.ClientCert);
      if AOptions.ClientKey <> '' then
        LContext.LoadPrivateKey(AOptions.ClientKey);
    end;
    
    // 创建连接
    LConnection := LContext.CreateConnection;
    if LConnection = nil then
    begin
      Result.ErrorMessage := '创建连接失败';
      Exit;
    end;
    
    // 连接到服务器
    LConnection.Connect(LHost, LPort);
    
    // 构建请求
    LRequest := BuildRequest(AMethod, LHost, LPath, ABody, AOptions.Headers);
    
    // 发送请求
    if LConnection.Write(LRequest[1], Length(LRequest)) <> Length(LRequest) then
    begin
      Result.ErrorMessage := '发送请求失败';
      Exit;
    end;
    
    // 读取响应
    LResponse := TMemoryStream.Create;
    try
      repeat
        try
          LBytesRead := LConnection.Read(LBuffer[0], Length(LBuffer));
          if LBytesRead > 0 then
            LResponse.Write(LBuffer[0], LBytesRead);
        except
          Break;
        end;
      until LBytesRead = 0;
      
      if LResponse.Size > 0 then
      begin
        SetLength(LRawResponse, LResponse.Size);
        LResponse.Position := 0;
        LResponse.Read(LRawResponse[1], LResponse.Size);
        
        // 解析响应
        Result := ParseResponse(LRawResponse);
      end
      else
      begin
        Result.ErrorMessage := '未收到响应';
      end;
    finally
      LResponse.Free;
    end;
    
  except
    on E: ESSLException do
      Result.ErrorMessage := 'SSL错误: ' + E.Message;
    on E: Exception do
      Result.ErrorMessage := '错误: ' + E.Message;
  end;
end;

{ 简化方法 }

class function TSimpleHTTPSClient.Get(const AURL: string): string;
var
  LResponse: THTTPResponse;
  LOptions: THTTPSOptions;
begin
  LOptions := DefaultOptions;
  try
    LResponse := GetEx(AURL, LOptions);
    if LResponse.Success then
      Result := LResponse.Body
    else
      raise EHTTPSClientException.Create(
        'HTTP错误: ' + LResponse.ErrorMessage, 
        LResponse.StatusCode);
  finally
    LOptions.Headers.Free;
    LResponse.Headers.Free;
  end;
end;

class function TSimpleHTTPSClient.Post(const AURL: string; const AData: string): string;
var
  LResponse: THTTPResponse;
  LOptions: THTTPSOptions;
begin
  LOptions := DefaultOptions;
  try
    LResponse := PostEx(AURL, AData, LOptions);
    if LResponse.Success then
      Result := LResponse.Body
    else
      raise EHTTPSClientException.Create(
        'HTTP错误: ' + LResponse.ErrorMessage,
        LResponse.StatusCode);
  finally
    LOptions.Headers.Free;
    LResponse.Headers.Free;
  end;
end;

{ 完整方法 }

class function TSimpleHTTPSClient.GetEx(const AURL: string; 
  const AOptions: THTTPSOptions): THTTPResponse;
begin
  Result := DoRequest(httpGET, AURL, '', AOptions);
end;

class function TSimpleHTTPSClient.PostEx(const AURL, AData: string;
  const AOptions: THTTPSOptions): THTTPResponse;
begin
  Result := DoRequest(httpPOST, AURL, AData, AOptions);
end;

class function TSimpleHTTPSClient.PutEx(const AURL, AData: string;
  const AOptions: THTTPSOptions): THTTPResponse;
begin
  Result := DoRequest(httpPUT, AURL, AData, AOptions);
end;

class function TSimpleHTTPSClient.DeleteEx(const AURL: string;
  const AOptions: THTTPSOptions): THTTPResponse;
begin
  Result := DoRequest(httpDELETE, AURL, '', AOptions);
end;

{ 文件操作 }

class function TSimpleHTTPSClient.Download(const AURL, AFilePath: string;
  AProgressCallback: TProgressCallback): Boolean;
var
  LResponse: THTTPResponse;
  LOptions: THTTPSOptions;
  LStream: TFileStream;
begin
  Result := False;
  LOptions := DefaultOptions;
  try
    LResponse := GetEx(AURL, LOptions);
    if LResponse.Success then
    begin
      LStream := TFileStream.Create(AFilePath, fmCreate);
      try
        if Length(LResponse.Body) > 0 then
          LStream.Write(LResponse.Body[1], Length(LResponse.Body));
        Result := True;
      finally
        LStream.Free;
      end;
    end;
  finally
    LOptions.Headers.Free;
    LResponse.Headers.Free;
  end;
end;

class function TSimpleHTTPSClient.Upload(const AURL, AFilePath: string): Boolean;
var
  LResponse: THTTPResponse;
  LOptions: THTTPSOptions;
  LStream: TFileStream;
  LData: string;
begin
  Result := False;
  
  if not FileExists(AFilePath) then
    Exit;
  
  LStream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LData, LStream.Size);
    if LStream.Size > 0 then
      LStream.Read(LData[1], LStream.Size);
  finally
    LStream.Free;
  end;
  
  LOptions := DefaultOptions;
  try
    LResponse := PostEx(AURL, LData, LOptions);
    Result := LResponse.Success;
  finally
    LOptions.Headers.Free;
    LResponse.Headers.Free;
  end;
end;

end.


