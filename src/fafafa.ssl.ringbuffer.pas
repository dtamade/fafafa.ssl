unit fafafa.ssl.ringbuffer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs;

type
  { TRingBuffer - 高性能环形缓冲区实现 }
  TRingBuffer = class
  private
    FBuffer: PByte;
    FCapacity: Integer;
    FMask: Integer;      // 用于快速取模运算 (capacity - 1)
    FReadPos: Integer;   // 读位置
    FWritePos: Integer;  // 写位置
    FSize: Integer;      // 当前数据大小
    FLock: TCriticalSection;
    
    function GetAvailable: Integer;
    function GetFreeSpace: Integer;
  public
    constructor Create(ACapacityPowerOf2: Integer = 16); // 2^16 = 65536 bytes
    destructor Destroy; override;
    
    // 写入数据到缓冲区
    function Write(const AData; ACount: Integer): Integer;
    // 从缓冲区读取数据
    function Read(var AData; ACount: Integer): Integer;
    // 查看数据但不移除（用于预览）
    function Peek(var AData; ACount: Integer; AOffset: Integer = 0): Integer;
    // 跳过指定字节数
    function Skip(ACount: Integer): Integer;
    // 清空缓冲区
    procedure Clear;
    
    // 零拷贝接口 - 获取可读的连续内存块
    procedure GetReadBuffer(out ABuffer: PByte; out ASize: Integer);
    // 零拷贝接口 - 确认已读取的字节数
    procedure ConfirmRead(ACount: Integer);
    
    // 零拷贝接口 - 获取可写的连续内存块
    procedure GetWriteBuffer(out ABuffer: PByte; out ASize: Integer);
    // 零拷贝接口 - 确认已写入的字节数
    procedure ConfirmWrite(ACount: Integer);
    
    property Available: Integer read GetAvailable;
    property FreeSpace: Integer read GetFreeSpace;
    property Capacity: Integer read FCapacity;
  end;

implementation

{ TRingBuffer }

constructor TRingBuffer.Create(ACapacityPowerOf2: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  
  // 确保容量是2的幂，这样可以用位运算代替取模
  FCapacity := 1 shl ACapacityPowerOf2;
  FMask := FCapacity - 1;
  
  // 分配内存
  GetMem(FBuffer, FCapacity);
  FillChar(FBuffer^, FCapacity, 0);
  
  FReadPos := 0;
  FWritePos := 0;
  FSize := 0;
end;

destructor TRingBuffer.Destroy;
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer);
  FLock.Free;
  inherited;
end;

function TRingBuffer.GetAvailable: Integer;
begin
  FLock.Enter;
  try
    Result := FSize;
  finally
    FLock.Leave;
  end;
end;

function TRingBuffer.GetFreeSpace: Integer;
begin
  FLock.Enter;
  try
    Result := FCapacity - FSize;
  finally
    FLock.Leave;
  end;
end;

function TRingBuffer.Write(const AData; ACount: Integer): Integer;
var
  BytesToWrite: Integer;
  FirstPart, SecondPart: Integer;
  DataPtr: PByte;
begin
  if ACount <= 0 then
    Exit(0);
    
  FLock.Enter;
  try
    // 计算实际可写入的字节数
    BytesToWrite := ACount;
    if BytesToWrite > (FCapacity - FSize) then
      BytesToWrite := FCapacity - FSize;
    
    if BytesToWrite = 0 then
      Exit(0);
    
    DataPtr := @AData;
    
    // 计算第一部分：从写位置到缓冲区末尾
    FirstPart := FCapacity - FWritePos;
    if FirstPart > BytesToWrite then
      FirstPart := BytesToWrite;
    
    // 复制第一部分
    Move(DataPtr^, (FBuffer + FWritePos)^, FirstPart);
    
    // 如果需要，复制第二部分（环绕到缓冲区开头）
    SecondPart := BytesToWrite - FirstPart;
    if SecondPart > 0 then
    begin
      Move((DataPtr + FirstPart)^, FBuffer^, SecondPart);
    end;
    
    // 更新写位置和大小
    FWritePos := (FWritePos + BytesToWrite) and FMask;
    Inc(FSize, BytesToWrite);
    
    Result := BytesToWrite;
  finally
    FLock.Leave;
  end;
end;

function TRingBuffer.Read(var AData; ACount: Integer): Integer;
var
  BytesToRead: Integer;
  FirstPart, SecondPart: Integer;
  DataPtr: PByte;
begin
  if ACount <= 0 then
    Exit(0);
    
  FLock.Enter;
  try
    // 计算实际可读取的字节数
    BytesToRead := ACount;
    if BytesToRead > FSize then
      BytesToRead := FSize;
    
    if BytesToRead = 0 then
      Exit(0);
    
    DataPtr := @AData;
    
    // 计算第一部分：从读位置到缓冲区末尾
    FirstPart := FCapacity - FReadPos;
    if FirstPart > BytesToRead then
      FirstPart := BytesToRead;
    
    // 复制第一部分
    Move((FBuffer + FReadPos)^, DataPtr^, FirstPart);
    
    // 如果需要，复制第二部分（从缓冲区开头）
    SecondPart := BytesToRead - FirstPart;
    if SecondPart > 0 then
    begin
      Move(FBuffer^, (DataPtr + FirstPart)^, SecondPart);
    end;
    
    // 更新读位置和大小
    FReadPos := (FReadPos + BytesToRead) and FMask;
    Dec(FSize, BytesToRead);
    
    Result := BytesToRead;
  finally
    FLock.Leave;
  end;
end;

function TRingBuffer.Peek(var AData; ACount: Integer; AOffset: Integer): Integer;
var
  BytesToRead: Integer;
  FirstPart, SecondPart: Integer;
  DataPtr: PByte;
  PeekPos: Integer;
begin
  if (ACount <= 0) or (AOffset < 0) then
    Exit(0);
    
  FLock.Enter;
  try
    // 检查偏移是否超出可用数据
    if AOffset >= FSize then
      Exit(0);
    
    // 计算实际可读取的字节数
    BytesToRead := ACount;
    if BytesToRead > (FSize - AOffset) then
      BytesToRead := FSize - AOffset;
    
    if BytesToRead = 0 then
      Exit(0);
    
    DataPtr := @AData;
    PeekPos := (FReadPos + AOffset) and FMask;
    
    // 计算第一部分
    FirstPart := FCapacity - PeekPos;
    if FirstPart > BytesToRead then
      FirstPart := BytesToRead;
    
    // 复制第一部分
    Move((FBuffer + PeekPos)^, DataPtr^, FirstPart);
    
    // 如果需要，复制第二部分
    SecondPart := BytesToRead - FirstPart;
    if SecondPart > 0 then
    begin
      Move(FBuffer^, (DataPtr + FirstPart)^, SecondPart);
    end;
    
    Result := BytesToRead;
  finally
    FLock.Leave;
  end;
end;

function TRingBuffer.Skip(ACount: Integer): Integer;
begin
  if ACount <= 0 then
    Exit(0);
    
  FLock.Enter;
  try
    Result := ACount;
    if Result > FSize then
      Result := FSize;
    
    FReadPos := (FReadPos + Result) and FMask;
    Dec(FSize, Result);
  finally
    FLock.Leave;
  end;
end;

procedure TRingBuffer.Clear;
begin
  FLock.Enter;
  try
    FReadPos := 0;
    FWritePos := 0;
    FSize := 0;
  finally
    FLock.Leave;
  end;
end;

procedure TRingBuffer.GetReadBuffer(out ABuffer: PByte; out ASize: Integer);
var
  ContiguousSize: Integer;
begin
  FLock.Enter;
  try
    if FSize = 0 then
    begin
      ABuffer := nil;
      ASize := 0;
      Exit;
    end;
    
    // 返回从读位置开始的连续内存块
    ABuffer := FBuffer + FReadPos;
    
    // 计算连续可读的大小
    ContiguousSize := FCapacity - FReadPos;
    if ContiguousSize > FSize then
      ContiguousSize := FSize;
    
    ASize := ContiguousSize;
  finally
    FLock.Leave;
  end;
end;

procedure TRingBuffer.ConfirmRead(ACount: Integer);
begin
  if ACount <= 0 then
    Exit;
    
  FLock.Enter;
  try
    if ACount > FSize then
      ACount := FSize;
    
    FReadPos := (FReadPos + ACount) and FMask;
    Dec(FSize, ACount);
  finally
    FLock.Leave;
  end;
end;

procedure TRingBuffer.GetWriteBuffer(out ABuffer: PByte; out ASize: Integer);
var
  ContiguousSize: Integer;
begin
  FLock.Enter;
  try
    if FSize >= FCapacity then
    begin
      ABuffer := nil;
      ASize := 0;
      Exit;
    end;
    
    // 返回从写位置开始的连续内存块
    ABuffer := FBuffer + FWritePos;
    
    // 计算连续可写的大小
    ContiguousSize := FCapacity - FWritePos;
    if ContiguousSize > (FCapacity - FSize) then
      ContiguousSize := FCapacity - FSize;
    
    ASize := ContiguousSize;
  finally
    FLock.Leave;
  end;
end;

procedure TRingBuffer.ConfirmWrite(ACount: Integer);
begin
  if ACount <= 0 then
    Exit;
    
  FLock.Enter;
  try
    if ACount > (FCapacity - FSize) then
      ACount := FCapacity - FSize;
    
    FWritePos := (FWritePos + ACount) and FMask;
    Inc(FSize, ACount);
  finally
    FLock.Leave;
  end;
end;

end.