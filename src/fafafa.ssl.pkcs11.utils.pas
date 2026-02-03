unit fafafa.ssl.pkcs11.utils;

{******************************************************************************}
{                                                                              }
{  fafafa.ssl - PKCS#11 Utility Functions                                     }
{                                                                              }
{  Purpose: Helper functions for PKCS#11 operations                           }
{                                                                              }
{  Features:                                                                   }
{    - Token enumeration and discovery                                        }
{    - Key enumeration within tokens                                          }
{    - PKCS#11 module loading and initialization                              }
{    - Slot and token information retrieval                                   }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.pkcs11.api,
  fafafa.ssl.pkcs11.loader;

type
  { TPKCS11Utils - Utility functions for PKCS#11 operations }
  TPKCS11Utils = class
  public
    { Enumerate all available slots
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        ATokenPresent: If True, only return slots with tokens present
        
      Returns:
        List of slot information
    }
    class function EnumerateSlots(const AModulePath: string; ATokenPresent: Boolean = True): TArray<TPKCS11SlotInfo>;
    
    { Enumerate all tokens
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        
      Returns:
        List of token information
    }
    class function EnumerateTokens(const AModulePath: string): TArray<TPKCS11TokenInfo>;
    
    { Find token by label
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        ATokenLabel: Token label to search for
        
      Returns:
        Token information if found
        
      Raises:
        EPKCS11Exception if token not found
    }
    class function FindTokenByLabel(const AModulePath: string; const ATokenLabel: string): TPKCS11TokenInfo;
    
    { Find slot by ID
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        ASlotID: Slot ID to search for
        
      Returns:
        Slot information if found
        
      Raises:
        EPKCS11Exception if slot not found
    }
    class function FindSlotByID(const AModulePath: string; ASlotID: CK_SLOT_ID): TPKCS11SlotInfo;
    
    { Enumerate keys in token
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        ASlotID: Slot ID containing the token
        APIN: PIN for token access (optional)
        
      Returns:
        List of key information
    }
    class function EnumerateKeys(const AModulePath: string; ASlotID: CK_SLOT_ID; const APIN: string = ''): TArray<TPKCS11KeyInfo>;
    
    { Find key by label
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        ASlotID: Slot ID containing the token
        AKeyLabel: Key label to search for
        APIN: PIN for token access (optional)
        
      Returns:
        Key information if found
        
      Raises:
        EPKCS11Exception if key not found
    }
    class function FindKeyByLabel(const AModulePath: string; ASlotID: CK_SLOT_ID; const AKeyLabel: string; const APIN: string = ''): TPKCS11KeyInfo;
    
    { Get PKCS#11 module information
      
      Parameters:
        AModulePath: Path to PKCS#11 module
        
      Returns:
        Module information string
    }
    class function GetModuleInfo(const AModulePath: string): string;
  end;

implementation

{ TPKCS11Utils }

class function TPKCS11Utils.EnumerateSlots(const AModulePath: string; ATokenPresent: Boolean): TArray<TPKCS11SlotInfo>;
var
  Loader: TPKCS11Loader;
  SlotList: PCK_SLOT_ID_Array;
  SlotCount: CK_ULONG;
  SlotInfo: CK_SLOT_INFO;
  I: Integer;
  RV: CK_RV;
  ResultList: TList<TPKCS11SlotInfo>;
begin
  SetLength(Result, 0);
  ResultList := TList<TPKCS11SlotInfo>.Create;
  try
    Loader := TPKCS11Loader.Create(AModulePath);
    try
      // Get slot count
      SlotCount := 0;
      RV := Loader.FunctionList^.C_GetSlotList(CK_BBOOL(ATokenPresent), nil, @SlotCount);
      if RV <> CKR_OK then
        raise EPKCS11Exception.Create('Failed to get slot count', RV);
      
      if SlotCount = 0 then
        Exit;
      
      // Get slot list
      GetMem(SlotList, SlotCount * SizeOf(CK_SLOT_ID));
      try
        RV := Loader.FunctionList^.C_GetSlotList(CK_BBOOL(ATokenPresent), SlotList, @SlotCount);
        if RV <> CKR_OK then
          raise EPKCS11Exception.Create('Failed to get slot list', RV);
        
        // Get slot information
        for I := 0 to Integer(SlotCount) - 1 do
        begin
          RV := Loader.FunctionList^.C_GetSlotInfo(SlotList^[I], @SlotInfo);
          if RV = CKR_OK then
            ResultList.Add(TPKCS11SlotInfo.FromCK(SlotInfo, SlotList^[I]));
        end;
      finally
        FreeMem(SlotList);
      end;
      
      Result := ResultList.ToArray;
    finally
      Loader.Free;
    end;
  finally
    ResultList.Free;
  end;
end;

class function TPKCS11Utils.EnumerateTokens(const AModulePath: string): TArray<TPKCS11TokenInfo>;
var
  Slots: TArray<TPKCS11SlotInfo>;
  Loader: TPKCS11Loader;
  TokenInfo: CK_TOKEN_INFO;
  I: Integer;
  RV: CK_RV;
  ResultList: TList<TPKCS11TokenInfo>;
begin
  SetLength(Result, 0);
  ResultList := TList<TPKCS11TokenInfo>.Create;
  try
    // Get all slots with tokens present
    Slots := EnumerateSlots(AModulePath, True);
    
    if Length(Slots) = 0 then
      Exit;
    
    Loader := TPKCS11Loader.Create(AModulePath);
    try
      // Get token information for each slot
      for I := 0 to High(Slots) do
      begin
        RV := Loader.FunctionList^.C_GetTokenInfo(Slots[I].SlotID, @TokenInfo);
        if RV = CKR_OK then
          ResultList.Add(TPKCS11TokenInfo.FromCK(TokenInfo, Slots[I].SlotID));
      end;
      
      Result := ResultList.ToArray;
    finally
      Loader.Free;
    end;
  finally
    ResultList.Free;
  end;
end;

class function TPKCS11Utils.FindTokenByLabel(const AModulePath: string; const ATokenLabel: string): TPKCS11TokenInfo;
var
  Tokens: TArray<TPKCS11TokenInfo>;
  I: Integer;
begin
  Tokens := EnumerateTokens(AModulePath);
  
  for I := 0 to High(Tokens) do
  begin
    if Tokens[I].Label = ATokenLabel then
    begin
      Result := Tokens[I];
      Exit;
    end;
  end;
  
  raise EPKCS11Exception.Create(
    Format('Token not found: %s', [ATokenLabel]),
    CKR_TOKEN_NOT_PRESENT);
end;

class function TPKCS11Utils.FindSlotByID(const AModulePath: string; ASlotID: CK_SLOT_ID): TPKCS11SlotInfo;
var
  Slots: TArray<TPKCS11SlotInfo>;
  I: Integer;
begin
  Slots := EnumerateSlots(AModulePath, False);
  
  for I := 0 to High(Slots) do
  begin
    if Slots[I].SlotID = ASlotID then
    begin
      Result := Slots[I];
      Exit;
    end;
  end;
  
  raise EPKCS11Exception.Create(
    Format('Slot not found: %d', [ASlotID]),
    CKR_SLOT_ID_INVALID);
end;

class function TPKCS11Utils.EnumerateKeys(const AModulePath: string; ASlotID: CK_SLOT_ID; const APIN: string): TArray<TPKCS11KeyInfo>;
var
  Loader: TPKCS11Loader;
  Session: CK_SESSION_HANDLE;
  Template: array[0..0] of CK_ATTRIBUTE;
  ObjectCount: CK_ULONG;
  ObjectHandle: CK_OBJECT_HANDLE;
  KeyInfo: TPKCS11KeyInfo;
  RV: CK_RV;
  ResultList: TList<TPKCS11KeyInfo>;
  KeyType: CK_KEY_TYPE;
  KeyLabel: array[0..255] of AnsiChar;
  KeyLabelLen: CK_ULONG;
  PINAnsi: AnsiString;
begin
  SetLength(Result, 0);
  ResultList := TList<TPKCS11KeyInfo>.Create;
  try
    Loader := TPKCS11Loader.Create(AModulePath);
    try
      // Open session
      RV := Loader.FunctionList^.C_OpenSession(ASlotID, CKF_SERIAL_SESSION, nil, nil, @Session);
      if RV <> CKR_OK then
        raise EPKCS11Exception.Create('Failed to open session', RV);
      
      try
        // Login if PIN provided
        if APIN <> '' then
        begin
          PINAnsi := AnsiString(APIN);
          RV := Loader.FunctionList^.C_Login(Session, CKU_USER, PAnsiChar(PINAnsi), Length(PINAnsi));
          if (RV <> CKR_OK) and (RV <> CKR_USER_ALREADY_LOGGED_IN) then
            raise EPKCS11Exception.Create('Failed to login', RV);
        end;
        
        // Find all private key objects
        Template[0].attrType := CKA_CLASS;
        Template[0].pValue := @CKO_PRIVATE_KEY;
        Template[0].ulValueLen := SizeOf(CK_OBJECT_CLASS);
        
        RV := Loader.FunctionList^.C_FindObjectsInit(Session, @Template[0], 1);
        if RV <> CKR_OK then
          raise EPKCS11Exception.Create('Failed to initialize object search', RV);
        
        try
          // Enumerate all private keys
          while True do
          begin
            ObjectCount := 0;
            RV := Loader.FunctionList^.C_FindObjects(Session, @ObjectHandle, 1, @ObjectCount);
            if (RV <> CKR_OK) or (ObjectCount = 0) then
              Break;
            
            // Get key information
            FillChar(KeyInfo, SizeOf(KeyInfo), 0);
            KeyInfo.Handle := ObjectHandle;
            
            // Get key type
            Template[0].attrType := CKA_KEY_TYPE;
            Template[0].pValue := @KeyType;
            Template[0].ulValueLen := SizeOf(CK_KEY_TYPE);
            RV := Loader.FunctionList^.C_GetAttributeValue(Session, ObjectHandle, @Template[0], 1);
            if RV = CKR_OK then
              KeyInfo.KeyType := PKCS11KeyTypeFromCK(KeyType);
            
            // Get key label
            FillChar(KeyLabel, SizeOf(KeyLabel), 0);
            KeyLabelLen := SizeOf(KeyLabel);
            Template[0].attrType := CKA_LABEL;
            Template[0].pValue := @KeyLabel[0];
            Template[0].ulValueLen := KeyLabelLen;
            RV := Loader.FunctionList^.C_GetAttributeValue(Session, ObjectHandle, @Template[0], 1);
            if RV = CKR_OK then
              KeyInfo.KeyLabel := TrimPKCS11String(KeyLabel);
            
            ResultList.Add(KeyInfo);
          end;
        finally
          Loader.FunctionList^.C_FindObjectsFinal(Session);
        end;
      finally
        Loader.FunctionList^.C_CloseSession(Session);
      end;
      
      Result := ResultList.ToArray;
    finally
      Loader.Free;
    end;
  finally
    ResultList.Free;
  end;
end;

class function TPKCS11Utils.FindKeyByLabel(const AModulePath: string; ASlotID: CK_SLOT_ID; const AKeyLabel: string; const APIN: string): TPKCS11KeyInfo;
var
  Keys: TArray<TPKCS11KeyInfo>;
  I: Integer;
begin
  Keys := EnumerateKeys(AModulePath, ASlotID, APIN);
  
  for I := 0 to High(Keys) do
  begin
    if Keys[I].KeyLabel = AKeyLabel then
    begin
      Result := Keys[I];
      Exit;
    end;
  end;
  
  raise EPKCS11Exception.Create(
    Format('Key not found: %s', [AKeyLabel]),
    CKR_KEY_HANDLE_INVALID);
end;

class function TPKCS11Utils.GetModuleInfo(const AModulePath: string): string;
var
  Loader: TPKCS11Loader;
  Info: CK_INFO;
  RV: CK_RV;
begin
  Loader := TPKCS11Loader.Create(AModulePath);
  try
    RV := Loader.FunctionList^.C_GetInfo(@Info);
    if RV <> CKR_OK then
      raise EPKCS11Exception.Create('Failed to get module info', RV);
    
    Result := Format('PKCS#11 Module Information:'#13#10 +
                     '  Cryptoki Version: %d.%d'#13#10 +
                     '  Manufacturer: %s'#13#10 +
                     '  Library Description: %s'#13#10 +
                     '  Library Version: %d.%d',
                     [Info.cryptokiVersion.major, Info.cryptokiVersion.minor,
                      TrimPKCS11String(Info.manufacturerID),
                      TrimPKCS11String(Info.libraryDescription),
                      Info.libraryVersion.major, Info.libraryVersion.minor]);
  finally
    Loader.Free;
  end;
end;

end.
