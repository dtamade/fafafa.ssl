unit fafafa.ssl.http.json;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{
  JSON HTTP 客户端
  
  基于 TSimpleHTTPSClient 的 JSON API 扩展
  让 REST API 调用变得简单
  
  示例：
    var LData: TJSONObject;
    LData := TJSONHTTPClient.GetJSON('https://api.example.com/users');
}

interface

uses
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  SysUtils, Classes, fpjson, jsonparser,
  fafafa.ssl.http.simple;

type
  { JSON HTTP 客户端 }
  TJSONHTTPClient = class
  public
    { GET 请求并解析JSON
      参数：URL
      返回：JSON对象（调用者负责释放） }
    class function GetJSON(const AURL: string): TJSONObject;
    
    { POST JSON 数据并解析响应
      参数：URL, JSON对象
      返回：JSON对象（调用者负责释放） }
    class function PostJSON(const AURL: string; AData: TJSONObject): TJSONObject; overload;
    
    { POST JSON 字符串并解析响应
      参数：URL, JSON字符串
      返回：JSON对象（调用者负责释放） }
    class function PostJSON(const AURL: string; const AData: string): TJSONObject; overload;
    
    { PUT JSON 数据 }
    class function PutJSON(const AURL: string; AData: TJSONObject): TJSONObject;
    
    { DELETE 请求并解析JSON }
    class function DeleteJSON(const AURL: string): TJSONObject;
    
    { GET 请求返回JSON数组
      参数：URL
      返回：JSON数组（调用者负责释放） }
    class function GetJSONArray(const AURL: string): TJSONArray;
    
    { 工具方法：解析JSON字符串 }
    class function ParseJSON(const AJSONString: string): TJSONData;
    
    { 工具方法：JSON对象转字符串 }
    class function JSONToString(AData: TJSONData): string;
  end;

implementation

{ TJSONHTTPClient }

class function TJSONHTTPClient.GetJSON(const AURL: string): TJSONObject;
var
  LResponse: string;
  LOptions: THTTPSOptions;
  LHTTPResponse: THTTPResponse;
  LJSONData: TJSONData;
begin
  Result := nil;
  
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LHTTPResponse := TSimpleHTTPSClient.GetEx(AURL, LOptions);
    try
      if not LHTTPResponse.Success then
        raise EHTTPSClientException.Create(
          'HTTP Error: ' + IntToStr(LHTTPResponse.StatusCode),
          LHTTPResponse.StatusCode
        );
      
      if LHTTPResponse.Body = '' then
        raise ESSLException.Create('Empty response body');
      
      LJSONData := GetJSON(LHTTPResponse.Body);
      if LJSONData is TJSONObject then
        Result := TJSONObject(LJSONData)
      else
      begin
        LJSONData.Free;
        raise ESSLException.Create('Response is not a JSON object');
      end;
      
    finally
      LHTTPResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

class function TJSONHTTPClient.PostJSON(
  const AURL: string;
  AData: TJSONObject
): TJSONObject;
var
  LJSONString: string;
begin
  LJSONString := AData.AsJSON;
  Result := PostJSON(AURL, LJSONString);
end;

class function TJSONHTTPClient.PostJSON(
  const AURL: string;
  const AData: string
): TJSONObject;
var
  LOptions: THTTPSOptions;
  LHTTPResponse: THTTPResponse;
  LJSONData: TJSONData;
begin
  Result := nil;
  
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    // 设置Content-Type为JSON
    LOptions.Headers.Add('Content-Type: application/json');
    
    LHTTPResponse := TSimpleHTTPSClient.PostEx(AURL, AData, LOptions);
    try
      if not LHTTPResponse.Success then
        raise EHTTPSClientException.Create(
          'HTTP Error: ' + IntToStr(LHTTPResponse.StatusCode),
          LHTTPResponse.StatusCode
        );
      
      if LHTTPResponse.Body = '' then
        Exit; // 某些API可能返回空响应
      
      LJSONData := GetJSON(LHTTPResponse.Body);
      if LJSONData is TJSONObject then
        Result := TJSONObject(LJSONData)
      else
      begin
        LJSONData.Free;
        raise ESSLException.Create('Response is not a JSON object');
      end;
      
    finally
      LHTTPResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

class function TJSONHTTPClient.PutJSON(
  const AURL: string;
  AData: TJSONObject
): TJSONObject;
var
  LOptions: THTTPSOptions;
  LHTTPResponse: THTTPResponse;
  LJSONData: TJSONData;
  LJSONString: string;
begin
  Result := nil;
  
  LJSONString := AData.AsJSON;
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LOptions.Headers.Add('Content-Type: application/json');
    
    LHTTPResponse := TSimpleHTTPSClient.PutEx(AURL, LJSONString, LOptions);
    try
      if not LHTTPResponse.Success then
        raise EHTTPSClientException.Create(
          'HTTP Error: ' + IntToStr(LHTTPResponse.StatusCode),
          LHTTPResponse.StatusCode
        );
      
      if LHTTPResponse.Body = '' then
        Exit;
      
      LJSONData := GetJSON(LHTTPResponse.Body);
      if LJSONData is TJSONObject then
        Result := TJSONObject(LJSONData)
      else
      begin
        LJSONData.Free;
        raise ESSLException.Create('Response is not a JSON object');
      end;
      
    finally
      LHTTPResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

class function TJSONHTTPClient.DeleteJSON(const AURL: string): TJSONObject;
var
  LOptions: THTTPSOptions;
  LHTTPResponse: THTTPResponse;
  LJSONData: TJSONData;
begin
  Result := nil;
  
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LHTTPResponse := TSimpleHTTPSClient.DeleteEx(AURL, LOptions);
    try
      if not LHTTPResponse.Success then
        raise EHTTPSClientException.Create(
          'HTTP Error: ' + IntToStr(LHTTPResponse.StatusCode),
          LHTTPResponse.StatusCode
        );
      
      if LHTTPResponse.Body = '' then
        Exit;
      
      LJSONData := GetJSON(LHTTPResponse.Body);
      if LJSONData is TJSONObject then
        Result := TJSONObject(LJSONData)
      else
      begin
        LJSONData.Free;
        raise ESSLException.Create('Response is not a JSON object');
      end;
      
    finally
      LHTTPResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

class function TJSONHTTPClient.GetJSONArray(const AURL: string): TJSONArray;
var
  LResponse: string;
  LOptions: THTTPSOptions;
  LHTTPResponse: THTTPResponse;
  LJSONData: TJSONData;
begin
  Result := nil;
  
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LHTTPResponse := TSimpleHTTPSClient.GetEx(AURL, LOptions);
    try
      if not LHTTPResponse.Success then
        raise EHTTPSClientException.Create(
          'HTTP Error: ' + IntToStr(LHTTPResponse.StatusCode),
          LHTTPResponse.StatusCode
        );
      
      if LHTTPResponse.Body = '' then
        raise ESSLException.Create('Empty response body');
      
      LJSONData := GetJSON(LHTTPResponse.Body);
      if LJSONData is TJSONArray then
        Result := TJSONArray(LJSONData)
      else
      begin
        LJSONData.Free;
        raise ESSLException.Create('Response is not a JSON array');
      end;
      
    finally
      LHTTPResponse.Headers.Free;
    end;
  finally
    LOptions.Headers.Free;
  end;
end;

class function TJSONHTTPClient.ParseJSON(const AJSONString: string): TJSONData;
begin
  Result := GetJSON(AJSONString);
end;

class function TJSONHTTPClient.JSONToString(AData: TJSONData): string;
begin
  if AData = nil then
    Result := ''
  else
    Result := AData.AsJSON;
end;

end.
