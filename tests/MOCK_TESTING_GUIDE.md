# Mock Testing Guide

## Overview

This guide explains how to write and use mock tests in the fafafa.ssl project. Mock tests are fast, isolated unit tests that don't depend on external libraries like OpenSSL.

## Why Mock Testing?

### Benefits

| Aspect | Mock Tests | Integration Tests |
|--------|-----------|-------------------|
| **Speed** | Instant (0.000s) | Slow (seconds) |
| **Dependencies** | None | Requires OpenSSL |
| **Isolation** | Complete | Partial |
| **Error Simulation** | Easy | Difficult |
| **CI/CD Friendly** | Yes | Sometimes |

### When to Use

- ✅ **Unit Tests** - Testing individual components in isolation
- ✅ **Error Paths** - Simulating failure scenarios
- ✅ **Fast Feedback** - During active development
- ✅ **CI/CD** - Quick validation in build pipelines

### When NOT to Use

- ❌ **Integration Testing** - Use real OpenSSL
- ❌ **Performance Testing** - Use real implementations
- ❌ **Final Validation** - Always verify with real libraries

## Project Structure

```
tests/
├── mocks/                           # Mock implementations
│   └── openssl_core_interface.pas   # Core interface + mock
├── unit/                            # Unit tests with mocks
│   ├── test_base.pas                # Base test class
│   ├── test_openssl_core_mock.pas   # Core mock tests
│   ├── test_mock.lpi                # Lazarus project
│   └── test_mock.lpr                # Test runner
└── integration/                     # Integration tests (future)
    └── test_openssl_real.pas        # Real OpenSSL tests
```

## Creating a Mock

### 1. Define the Interface

```pascal
type
  IYourInterface = interface
    ['{GUID-HERE}']
    function DoSomething(aParam: Integer): Boolean;
    function GetValue: string;
  end;
```

### 2. Create Mock Implementation

```pascal
type
  TYourInterfaceMock = class(TInterfacedObject, IYourInterface)
  private
    FCallCount: Integer;
    FShouldFail: Boolean;
    FCustomValue: string;
  public
    // Mock control methods
    procedure SetShouldFail(aValue: Boolean);
    procedure SetCustomValue(const aValue: string);
    function GetCallCount: Integer;
    
    // Interface implementation
    function DoSomething(aParam: Integer): Boolean;
    function GetValue: string;
  end;
```

### 3. Implement Mock Behavior

```pascal
function TYourInterfaceMock.DoSomething(aParam: Integer): Boolean;
begin
  Inc(FCallCount);
  Result := not FShouldFail;
end;

function TYourInterfaceMock.GetValue: string;
begin
  if FCustomValue <> '' then
    Result := FCustomValue
  else
    Result := 'Mock Value';
end;
```

## Writing Tests

### Test Structure (AAA Pattern)

```pascal
procedure TTestYourFeature.TestSomething_ShouldSucceed_WhenValid;
begin
  // Arrange (Setup)
  FMock.SetCustomValue('Expected');
  
  // Act (Execute)
  Result := FInterface.DoSomething(123);
  
  // Assert (Verify)
  AssertTrue('Should succeed', Result);
  AssertEquals('Should be called once', 1, FMock.GetCallCount);
end;
```

### Test Naming Convention

```pascal
// Pattern: Test<What>_Should<Behavior>_When<Condition>

procedure TestLoad_ShouldReturnTrue_WhenSuccessful;
procedure TestLoad_ShouldReturnFalse_WhenConfiguredToFail;
procedure TestGetVersion_ShouldReturnEmpty_WhenNotLoaded;
```

### SetUp and TearDown

```pascal
procedure TTestYourFeature.SetUp;
begin
  inherited SetUp;
  // Create fresh mock for each test
  FMock := TYourInterfaceMock.Create;
  FInterface := FMock as IYourInterface;
end;

procedure TTestYourFeature.TearDown;
begin
  // Clean up
  FInterface := nil;
  FMock := nil;
  inherited TearDown;
end;
```

## Example: Testing Core OpenSSL

### Complete Test Example

```pascal
unit test_openssl_core_mock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  test_base,
  openssl_core_interface;

type
  TTestOpenSSLCoreMock = class(TTestBase)
  private
    FCore: IOpenSSLCore;
    FMock: TOpenSSLCoreMock;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoad_ShouldReturnTrue_WhenSuccessful;
    procedure TestLoad_ShouldReturnFalse_WhenConfiguredToFail;
  end;

implementation

procedure TTestOpenSSLCoreMock.SetUp;
begin
  inherited SetUp;
  FMock := TOpenSSLCoreMock.Create;
  FCore := FMock as IOpenSSLCore;
end;

procedure TTestOpenSSLCoreMock.TearDown;
begin
  FCore := nil;
  FMock := nil;
  inherited TearDown;
end;

procedure TTestOpenSSLCoreMock.TestLoad_ShouldReturnTrue_WhenSuccessful;
var
  Result: Boolean;
begin
  // Given - mock configured for success by default
  
  // When
  Result := FCore.LoadLibrary;
  
  // Then
  AssertTrue('LoadLibrary should return True', Result);
  AssertTrue('IsLoaded should be True after load', FCore.IsLoaded);
end;

procedure TTestOpenSSLCoreMock.TestLoad_ShouldReturnFalse_WhenConfiguredToFail;
var
  Result: Boolean;
begin
  // Given
  FMock.SetShouldFailLoad(True);
  
  // When
  Result := FCore.LoadLibrary;
  
  // Then
  AssertFalse('Should return False when configured to fail', Result);
  AssertFalse('Should not be loaded after failed load', FCore.IsLoaded);
end;

initialization
  RegisterTest(TTestOpenSSLCoreMock);

end.
```

## Running Tests

### Compile and Run

```bash
# Build the test project
lazbuild --build-mode=Release tests/unit/test_mock.lpi

# Run tests
./bin/test_mock.exe

# Expected output:
# Time:00.000 N:16 E:0 F:0 I:0
# Number of run tests: 16
# Number of errors:    0
# Number of failures:  0
```

### Using Lazarus IDE

1. Open `tests/unit/test_mock.lpi`
2. Press F9 to run
3. View results in console

## Best Practices

### ✅ DO

1. **Keep Tests Simple**
   - One test = one behavior
   - Clear test names
   - Minimal setup

2. **Test Behavior, Not Implementation**
   ```pascal
   // Good: Test observable behavior
   AssertTrue('Should be loaded', FCore.IsLoaded);
   
   // Bad: Test internal state
   AssertEquals('Should set flag', True, FCore.FInternalFlag);
   ```

3. **Use AAA Pattern**
   - Arrange: Set up test data
   - Act: Execute the code
   - Assert: Verify results

4. **Test Edge Cases**
   ```pascal
   procedure TestDivide_ShouldThrowError_WhenZero;
   procedure TestArray_ShouldHandleEmpty_Gracefully;
   ```

### ❌ DON'T

1. **Don't Test Multiple Things**
   ```pascal
   // Bad: Too many assertions
   procedure TestEverything;
   begin
     Assert(Load);
     Assert(Process);
     Assert(Save);
     Assert(Close);
   end;
   ```

2. **Don't Depend on Order**
   ```pascal
   // Bad: Tests depend on each other
   procedure TestStep1; // Must run first
   procedure TestStep2; // Depends on Step1
   ```

3. **Don't Use Real Resources**
   ```pascal
   // Bad: Loading real OpenSSL in unit test
   LoadLibrary('libssl-3.dll');
   
   // Good: Using mock
   FMock := TOpenSSLCoreMock.Create;
   ```

## Mock Control Methods

### Common Patterns

```pascal
type
  TYourMock = class
  private
    FCallCount: Integer;
    FShouldFail: Boolean;
    FReturnValue: Variant;
  public
    // Behavior control
    procedure SetShouldFail(aValue: Boolean);
    procedure SetReturnValue(aValue: Variant);
    
    // Verification
    function GetCallCount: Integer;
    function WasCalled: Boolean;
    function GetLastParam: Variant;
    
    // Reset
    procedure Reset;
  end;
```

## Debugging Tests

### Common Issues

**Test Fails Unexpectedly**
```pascal
// Add debug output
WriteLn('Expected: ', Expected);
WriteLn('Actual: ', Actual);
WriteLn('Mock State: ', FMock.DebugState);
```

**Mock Not Behaving**
```pascal
// Verify mock setup
Assert(FMock <> nil, 'Mock should be created');
Assert(Supports(FMock, IYourInterface), 'Should support interface');
```

## Next Steps

- [ ] Add more mock implementations for crypto operations
- [ ] Create integration tests with real OpenSSL
- [ ] Set up automated test execution
- [ ] Add code coverage reporting

## Resources

- [FPCUnit Documentation](https://wiki.freepascal.org/fpcunit)
- [TDD Best Practices](../WARP.md#tdd-development-guidelines)
- [Project Structure](../README.md#project-structure)

---

**Remember**: Mock tests are for speed and isolation. Always verify critical functionality with integration tests using real OpenSSL.
