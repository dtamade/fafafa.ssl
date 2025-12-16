

class function TCertificateUtils.CompareX509Names(
  const AName1, AName2: string;
  ACaseInsensitive: Boolean
): Boolean;
var
  LName1, LName2: string;
  LComponents1, LComponents2: TStringList;
  i: Integer;
  
  function NormalizeDN(const ADN: string): string;
  var
    s: string;
  begin
    s := Trim(ADN);
    // 移除=号周围的空格
    s := StringReplace(s, ' = ', '=', [rfReplaceAll]);
    s := StringReplace(s, '= ', '=', [rfReplaceAll]);
    s := StringReplace(s, ' =', '=', [rfReplaceAll]);
    // 移除逗号后的空格
    s := StringReplace(s, ', ', ',', [rfReplaceAll]);
    
    if ACaseInsensitive then
      Result := LowerCase(s)
    else
      Result := s;
  end;
  
  procedure ParseDN(const ADN: string; AList: TStringList);
  var
    Components: TStringArray;
    i: Integer;
  begin
    AList.Clear;
    AList.Sorted := True; // 自动排序以忽略顺序
    AList.Duplicates := dupIgnore;
    
    Components := ADN.Split([',']);
    for i := 0 to Length(Components) - 1 do
      AList.Add(Trim(Components[i]));
  end;
  
begin
  Result := False;
  
  if AName1 = AName2 then
  begin
    Result := True;
    Exit;
  end;
  
  // 规范化DN
  LName1 := NormalizeDN(AName1);
  LName2 := NormalizeDN(AName2);
  
  // 标准化后直接比较
  if LName1 = LName2 then
  begin
    Result := True;
    Exit;
  end;
  
  // 解析组件并排序比较
  LComponents1 := TStringList.Create;
  LComponents2 := TStringList.Create;
  try
    ParseDN(LName1, LComponents1);
    ParseDN(LName2, LComponents2);
    
    if LComponents1.Count <> LComponents2.Count then
      Exit;
      
    Result := True;
    for i := 0 to LComponents1.Count - 1 do
    begin
      if LComponents1[i] <> LComponents2[i] then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    LComponents1.Free;
    LComponents2.Free;
  end;
end;

end.
