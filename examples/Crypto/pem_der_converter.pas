program pem_der_converter;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.cert.utils;

procedure PrintBanner;
begin
  WriteLn;
  WriteLn('=====================================');
  WriteLn('  PEM/DER Certificate Converter');
  WriteLn('  fafafa.ssl toolkit');
  WriteLn('=====================================');
  WriteLn;
end;

procedure PrintUsage;
begin
  WriteLn('Usage:');
  WriteLn('  pem_der_converter pem2der  <input.pem> <output.der>');
  WriteLn('  pem_der_converter der2pem  <input.der> <output.pem>');
  WriteLn;
end;

procedure ConvertPEMToDER(const AInputFile, AOutputFile: string);
var
  LPEM: string;
  LDER: TBytes;
  LStream: TFileStream;
begin
  WriteLn('Converting PEM to DER...');
  WriteLn('  Input : ', AInputFile);
  WriteLn('  Output: ', AOutputFile);
  
  LPEM := TCertificateUtils.LoadFromFile(AInputFile);
  LDER := TCertificateUtils.PEMToDER(LPEM);
  
  if Length(LDER) = 0 then
  begin
    WriteLn('ERROR: Conversion failed');
    Halt(1);
  end;
  
  LStream := TFileStream.Create(AOutputFile, fmCreate);
  try
    LStream.Write(LDER[0], Length(LDER));
    WriteLn('✓ Success! Converted ', Length(LDER), ' bytes');
  finally
    LStream.Free;
  end;
end;

procedure ConvertDERToPEM(const AInputFile, AOutputFile: string);
var
  LPEM: string;
  LDER: TBytes;
  LStream: TFileStream;
begin
  WriteLn('Converting DER to PEM...');
  WriteLn('  Input : ', AInputFile);
  WriteLn('  Output: ', AOutputFile);
  
  LStream := TFileStream.Create(AInputFile, fmOpenRead);
  try
    SetLength(LDER, LStream.Size);
    if LStream.Size > 0 then
      LStream.Read(LDER[0], LStream.Size);
  finally
    LStream.Free;
  end;
  
  LPEM := TCertificateUtils.DERToPEM(LDER);
  
  if LPEM = '' then
  begin
    WriteLn('ERROR: Conversion failed');
    Halt(1);
  end;
  
  if not TCertificateUtils.SaveToFile(AOutputFile, LPEM) then
  begin
    WriteLn('ERROR: Failed to save output file');
    Halt(1);
  end;
  
  WriteLn('✓ Success! Converted to PEM format');
end;

var
  LMode: string;
begin
  PrintBanner;
  
  if ParamCount < 3 then
  begin
    PrintUsage;
    Halt(1);
  end;
  
  LMode := LowerCase(ParamStr(1));
  
  try
    if LMode = 'pem2der' then
      ConvertPEMToDER(ParamStr(2), ParamStr(3))
    else if LMode = 'der2pem' then
      ConvertDERToPEM(ParamStr(2), ParamStr(3))
    else
    begin
      WriteLn('ERROR: Unknown mode: ', ParamStr(1));
      PrintUsage;
      Halt(1);
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
  
  WriteLn;
end.
