program example_streaming_operations;

{$mode objfpc}{$H+}

{**
 * Phase 2.3.4 - Streaming Operations Examples
 *
 * Demonstrates how to use TStreamingHasher and TStreamingCipher
 * for incremental processing of large data.
 *}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.crypto.utils;

{**
 * Example 1: Streaming Hash for Large File
 *
 * Problem: Need to hash a large file without loading it entirely into memory.
 * Solution: Use TStreamingHasher to process file in chunks.
 *}
procedure Example1_StreamingFileHash;
var
  LHasher: TStreamingHasher;
  LChunk: TBytes;
  LHash: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Example 1: Streaming Hash for Large File ===');
  WriteLn;

  LHasher := TStreamingHasher.Create(HASH_SHA256);
  try
    // Simulate reading file in 1KB chunks
    WriteLn('Processing file in chunks...');
    for I := 1 to 10 do
    begin
      // In real scenario, read chunk from file
      SetLength(LChunk, 1024);
      FillByte(LChunk[0], Length(LChunk), Byte(I mod 256));

      LHasher.Update(LChunk);
      Write('.');
    end;
    WriteLn;

    // Get final hash
    LHash := LHasher.Finalize;

    WriteLn('File hash (SHA256): ', TCryptoUtils.BytesToHex(LHash));
    WriteLn('Memory used: Only 1KB buffer (vs loading entire file)');
  finally
    LHasher.Free;
  end;
end;

{**
 * Example 2: Progressive UI Updates During Hashing
 *
 * Problem: Need to show progress while hashing large data.
 * Solution: Update UI after each chunk.
 *}
procedure Example2_ProgressiveHashing;
var
  LHasher: TStreamingHasher;
  LChunk: TBytes;
  LHash: TBytes;
  I, LTotalChunks: Integer;
  LProgress: Double;
begin
  WriteLn;
  WriteLn('=== Example 2: Progressive Hashing with Progress Updates ===');
  WriteLn;

  LTotalChunks := 20;
  LHasher := TStreamingHasher.Create(HASH_SHA256);
  try
    WriteLn('Hashing with progress updates:');

    for I := 1 to LTotalChunks do
    begin
      // Simulate data chunk
      SetLength(LChunk, 512);
      FillByte(LChunk[0], Length(LChunk), Byte(I));

      LHasher.Update(LChunk);

      // Update progress
      LProgress := (I / LTotalChunks) * 100;
      WriteLn(Format('  Progress: %.0f%% (%d/%d chunks)', [LProgress, I, LTotalChunks]));
    end;

    LHash := LHasher.Finalize;
    WriteLn;
    WriteLn('Hash complete: ', TCryptoUtils.BytesToHex(LHash));
  finally
    LHasher.Free;
  end;
end;

{**
 * Example 3: Reusable Hasher with Reset
 *
 * Problem: Need to hash multiple files with same algorithm.
 * Solution: Create hasher once, use Reset between files.
 *}
procedure Example3_ReusableHasher;
var
  LHasher: TStreamingHasher;
  LData: TBytes;
  LHash1, LHash2, LHash3: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Example 3: Reusable Hasher (Memory Efficient) ===');
  WriteLn;

  // Create hasher once
  LHasher := TStreamingHasher.Create(HASH_SHA256);
  try
    // Hash file 1
    SetLength(LData, 100);
    for I := 0 to 99 do
      LData[I] := Byte(1);
    LHasher.Update(LData);
    LHash1 := LHasher.Finalize;
    WriteLn('File 1 hash: ', TCryptoUtils.BytesToHex(LHash1));

    // Reset and hash file 2
    LHasher.Reset;
    for I := 0 to 99 do
      LData[I] := Byte(2);
    LHasher.Update(LData);
    LHash2 := LHasher.Finalize;
    WriteLn('File 2 hash: ', TCryptoUtils.BytesToHex(LHash2));

    // Reset and hash file 3
    LHasher.Reset;
    for I := 0 to 99 do
      LData[I] := Byte(3);
    LHasher.Update(LData);
    LHash3 := LHasher.Finalize;
    WriteLn('File 3 hash: ', TCryptoUtils.BytesToHex(LHash3));

    WriteLn;
    WriteLn('Benefit: Single hasher object reused for all files');
  finally
    LHasher.Free;
  end;
end;

{**
 * Example 4: Streaming Encryption for Large File
 *
 * Problem: Need to encrypt a large file without loading it all into memory.
 * Solution: Use TStreamingCipher to encrypt in chunks.
 *}
procedure Example4_StreamingEncryption;
var
  LCipher: TStreamingCipher;
  LKey, LIV: TBytes;
  LChunk, LEncChunk, LFinal, LTag: TBytes;
  I: Integer;
  LTotalEncrypted: Integer;
begin
  WriteLn;
  WriteLn('=== Example 4: Streaming Encryption ===');
  WriteLn;

  // Generate key and IV
  LKey := TCryptoUtils.GenerateKey(256);
  LIV := TCryptoUtils.GenerateIV(12);

  WriteLn('Encrypting large file in chunks...');

  LCipher := TStreamingCipher.CreateEncrypt(ENCRYPT_AES_256_GCM, LKey, LIV);
  try
    LTotalEncrypted := 0;

    // Process 5 chunks
    for I := 1 to 5 do
    begin
      // Simulate reading chunk from file
      SetLength(LChunk, 1024);
      FillByte(LChunk[0], Length(LChunk), Byte(I * 10));

      if LCipher.Update(LChunk, LEncChunk) then
      begin
        WriteLn(Format('  Chunk %d: %d bytes -> %d bytes encrypted',
          [I, Length(LChunk), Length(LEncChunk)]));
        LTotalEncrypted := LTotalEncrypted + Length(LEncChunk);

        // In real scenario: Write LEncChunk to output file
      end;
    end;

    // Finalize and get authentication tag
    if LCipher.Finalize(LFinal, LTag) then
    begin
      LTotalEncrypted := LTotalEncrypted + Length(LFinal);
      WriteLn;
      WriteLn(Format('Total encrypted: %d bytes', [LTotalEncrypted]));
      WriteLn('Authentication tag: ', TCryptoUtils.BytesToHex(LTag));
      WriteLn;
      WriteLn('Note: Tag must be stored/transmitted for decryption verification');
    end;
  finally
    LCipher.Free;
  end;
end;

{**
 * Example 5: Streaming Decryption with Authentication
 *
 * Problem: Decrypt large encrypted file with authentication.
 * Solution: Use TStreamingCipher for decryption, verify tag at end.
 *}
procedure Example5_StreamingDecryption;
var
  LEncCipher, LDecCipher: TStreamingCipher;
  LKey, LIV: TBytes;
  LChunk, LEncChunk, LDecChunk: TBytes;
  LEncFinal, LDecFinal, LTag: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Example 5: Streaming Decryption with Authentication ===');
  WriteLn;

  LKey := TCryptoUtils.GenerateKey(256);
  LIV := TCryptoUtils.GenerateIV(12);

  WriteLn('Step 1: Encrypt data...');

  // Encrypt
  LEncCipher := TStreamingCipher.CreateEncrypt(ENCRYPT_AES_256_GCM, LKey, LIV);
  try
    SetLength(LChunk, 2048);
    for I := 0 to 2047 do
      LChunk[I] := Byte(I mod 256);

    LEncCipher.Update(LChunk, LEncChunk);
    LEncCipher.Finalize(LEncFinal, LTag);

    WriteLn('  Encrypted successfully, tag obtained');
  finally
    LEncCipher.Free;
  end;

  WriteLn;
  WriteLn('Step 2: Decrypt and verify...');

  // Decrypt
  LDecCipher := TStreamingCipher.CreateDecrypt(ENCRYPT_AES_256_GCM, LKey, LIV);
  try
    LDecCipher.Update(LEncChunk, LDecChunk);

    if LDecCipher.Finalize(LDecFinal, LTag) then
    begin
      WriteLn('  ✓ Decryption successful');
      WriteLn('  ✓ Authentication verified (tag matches)');
      WriteLn('  ✓ Data integrity confirmed');
    end
    else
    begin
      WriteLn('  ✗ Authentication failed (tag mismatch)');
      WriteLn('  ✗ Data may be corrupted or tampered');
    end;
  finally
    LDecCipher.Free;
  end;
end;

{**
 * Example 6: Zero-Copy with TBytesView
 *
 * Problem: Large buffer already in memory, want to hash without copying.
 * Solution: Use UpdateView with TBytesView.
 *}
procedure Example6_ZeroCopyStreaming;
var
  LHasher: TStreamingHasher;
  LLargeBuffer: TBytes;
  LView1, LView2, LView3: TBytesView;
  LHash: TBytes;
  I: Integer;
begin
  WriteLn;
  WriteLn('=== Example 6: Zero-Copy Streaming with TBytesView ===');
  WriteLn;

  // Simulate large buffer (e.g., mapped file, network buffer)
  SetLength(LLargeBuffer, 30000);
  for I := 0 to 29999 do
    LLargeBuffer[I] := Byte(I mod 256);

  WriteLn('Processing 30KB buffer in 10KB chunks (zero-copy)...');

  LHasher := TStreamingHasher.Create(HASH_SHA256);
  try
    // Create views (no data copying)
    LView1 := TBytesView.FromBytes(LLargeBuffer).Slice(0, 10000);
    LView2 := TBytesView.FromBytes(LLargeBuffer).Slice(10000, 10000);
    LView3 := TBytesView.FromBytes(LLargeBuffer).Slice(20000, 10000);

    // Update using views (zero-copy)
    LHasher.UpdateView(LView1);
    WriteLn('  Chunk 1 processed (0-10000)');

    LHasher.UpdateView(LView2);
    WriteLn('  Chunk 2 processed (10000-20000)');

    LHasher.UpdateView(LView3);
    WriteLn('  Chunk 3 processed (20000-30000)');

    LHash := LHasher.Finalize;

    WriteLn;
    WriteLn('Hash: ', TCryptoUtils.BytesToHex(LHash));
    WriteLn('Memory copies: 0 (only views created)');
  finally
    LHasher.Free;
  end;
end;

{**
 * Example 7: Network Stream Processing
 *
 * Problem: Hash/encrypt data as it arrives from network.
 * Solution: Update hasher/cipher as packets arrive.
 *}
procedure Example7_NetworkStreamProcessing;
var
  LHasher: TStreamingHasher;
  LPacket: TBytes;
  LHash: TBytes;
  LPacketNum: Integer;
begin
  WriteLn;
  WriteLn('=== Example 7: Network Stream Processing ===');
  WriteLn;

  WriteLn('Simulating network packet reception...');

  LHasher := TStreamingHasher.Create(HASH_SHA256);
  try
    // Simulate receiving 10 packets
    for LPacketNum := 1 to 10 do
    begin
      // Simulate variable-size packet
      SetLength(LPacket, 100 + Random(900));
      FillByte(LPacket[0], Length(LPacket), Byte(LPacketNum));

      LHasher.Update(LPacket);

      WriteLn(Format('  Packet %d received and hashed (%d bytes)',
        [LPacketNum, Length(LPacket)]));
    end;

    WriteLn;
    WriteLn('All packets received, computing final hash...');

    LHash := LHasher.Finalize;

    WriteLn('Stream hash: ', TCryptoUtils.BytesToHex(LHash));
    WriteLn;
    WriteLn('Use case: Verify data integrity for streamed downloads');
  finally
    LHasher.Free;
  end;
end;

begin
  try
    WriteLn('═══════════════════════════════════════════════════════════════');
    WriteLn('  Phase 2.3.4: Streaming Operations Examples');
    WriteLn('═══════════════════════════════════════════════════════════════');

    Example1_StreamingFileHash;
    Example2_ProgressiveHashing;
    Example3_ReusableHasher;
    Example4_StreamingEncryption;
    Example5_StreamingDecryption;
    Example6_ZeroCopyStreaming;
    Example7_NetworkStreamProcessing;

    WriteLn;
    WriteLn('═══════════════════════════════════════════════════════════════');
    WriteLn('  All examples completed successfully!');
    WriteLn('═══════════════════════════════════════════════════════════════');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
