# Contributing to fafafa.ssl

Thank you for your interest in contributing to fafafa.ssl! This document provides guidelines for contributing to the project.

---

## 📋 Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Setup](#development-setup)
4. [Contribution Workflow](#contribution-workflow)
5. [Coding Standards](#coding-standards)
6. [Testing Requirements](#testing-requirements)
7. [Documentation](#documentation)
8. [Pull Request Process](#pull-request-process)
9. [Issue Reporting](#issue-reporting)

---

## 📜 Code of Conduct

### Our Pledge

We are committed to providing a welcoming and inclusive environment for all contributors.

### Standards

**Expected Behavior**:
- ✅ Respectful communication
- ✅ Constructive feedback
- ✅ Collaboration and openness
- ✅ Focus on what's best for the community

**Unacceptable Behavior**:
- ❌ Harassment or discrimination
- ❌ Trolling or inflammatory comments
- ❌ Personal attacks
- ❌ Publishing others' private information

---

## 🚀 Getting Started

### Prerequisites

- **Free Pascal**: 3.3.1 or higher
- **Git**: For version control
- **Basic knowledge of**:
  - Pascal/Object Pascal
  - SSL/TLS concepts
  - Git workflow

### Areas for Contribution

1. **Code**: Bug fixes, new features, optimizations
2. **Documentation**: Improvements, examples, tutorials
3. **Testing**: New tests, bug reproduction
4. **Examples**: Real-world usage examples
5. **Platform Support**: macOS, additional Linux distros
6. **Backends**: MbedTLS, WolfSSL implementations

---

## 💻 Development Setup

### 1. Fork and Clone

```bash
# Fork the repository on GitHub

# Clone your fork
git clone https://github.com/YOUR_USERNAME/fafafa.ssl.git
cd fafafa.ssl

# Add upstream remote
git remote add upstream https://github.com/ORIGINAL_OWNER/fafafa.ssl.git
```

### 2. Build the Project

#### Linux
```bash
chmod +x build_linux.sh
./build_linux.sh
```

#### Windows
```powershell
.\build_windows.ps1
```

### 3. Run Tests

```bash
# Linux
./run_tests_linux.sh

# Windows
.\run_all_tests.ps1
```

### 4. Verify Setup

All core tests should pass before you start contributing.

---

## 🔄 Contribution Workflow

### 1. Create a Branch

```bash
# Update your fork
git fetch upstream
git checkout main
git merge upstream/main

# Create feature branch
git checkout -b feature/your-feature-name
# Or for bug fixes
git checkout -b fix/bug-description
```

### Branch Naming Convention
- `feature/feature-name` - New features
- `fix/bug-name` - Bug fixes
- `docs/topic` - Documentation changes
- `test/test-name` - Test additions
- `refactor/component` - Code refactoring

### 2. Make Changes

- Write clean, well-documented code
- Follow coding standards (see below)
- Add tests for new functionality
- Update documentation

### 3. Commit Changes

```bash
git add .
git commit -m "type: description"
```

**Commit Message Format**:
```
type: brief description (50 chars or less)

Detailed explanation if needed (wrap at 72 characters).

- Bullet points for multiple changes
- Reference issues: Fixes #123
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Maintenance tasks
- `style`: Code style changes (formatting)

**Examples**:
```
feat: add MbedTLS backend support

fix: resolve memory leak in SSL context cleanup

docs: improve quick start guide with more examples

test: add comprehensive ECDH test coverage
```

### 4. Push Changes

```bash
git push origin feature/your-feature-name
```

### 5. Create Pull Request

- Go to GitHub and create a Pull Request
- Fill in the PR template
- Link related issues
- Request review from maintainers

---

## 📏 Coding Standards

### Pascal Style Guide

Follow the project's existing code style. Key points:

#### Naming Conventions

```pascal
// Constants: UPPER_CASE_WITH_UNDERSCORES
const
  MAX_BUFFER_SIZE = 4096;
  DEFAULT_TIMEOUT = 30;

// Types: PascalCase with T prefix
type
  TSSLContext = class
  TConnectionState = (csDisconnected, csConnecting, csConnected);
  
// Interfaces: PascalCase with I prefix
type
  ISSLLibrary = interface
  
// Variables: PascalCase
var
  ConnectionTimeout: Integer;
  BufferSize: Cardinal;
  
// Private fields: F prefix
private
  FSocket: TSocket;
  FTimeout: Integer;
  
// Parameters: a prefix
function DoSomething(aTimeout: Integer; aBuffer: PByte): Boolean;
```

#### Code Organization

```pascal
{$mode objfpc}{$H+}

unit fafafa.ssl.example;

interface

uses
  // Standard units first
  SysUtils, Classes,
  // Then project units
  fafafa.ssl.types,
  fafafa.ssl.intf;

type
  // Public types
  
const
  // Public constants
  
// Public functions/procedures

implementation

uses
  // Implementation-only units
  
type
  // Private types
  
const
  // Private constants
  
// Private functions/procedures

// Public function implementations

end.
```

#### Indentation and Formatting

- **Indentation**: 2 spaces (no tabs)
- **Line length**: Prefer < 100 characters
- **Begin/End**: Align vertically

```pascal
// Good
if Condition then
begin
  DoSomething;
  DoAnotherThing;
end;

// Also acceptable for single statements
if Condition then
  DoOneThing;
```

### Documentation

```pascal
/// <summary>
/// Initializes the SSL context with the specified parameters.
/// </summary>
/// <param name="aType">The type of SSL context to create</param>
/// <returns>True if initialization succeeds, False otherwise</returns>
function Initialize(aType: TSSLContextType): Boolean;
```

---

## 🧪 Testing Requirements

### Test Coverage

All contributions must include appropriate tests:

1. **New Features**: Add unit tests and integration tests
2. **Bug Fixes**: Add regression test to prevent reoccurrence
3. **Refactoring**: Ensure existing tests still pass

### Writing Tests

Follow the existing test structure:

```pascal
program test_your_feature;

{$mode objfpc}{$H+}

uses
  SysUtils,
  your_module;

var
  TotalTests, PassedTests, FailedTests: Integer;

procedure Test(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(TestName + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FailedTests);
  end;
end;

procedure TestYourFeature;
begin
  // Arrange
  // Act
  // Assert
  Test('Feature should work', Result = Expected);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;
  
  WriteLn('Testing: Your Feature');
  WriteLn('==============================');
  
  TestYourFeature;
  
  WriteLn('==============================');
  WriteLn(Format('Results: %d/%d passed', [PassedTests, TotalTests]));
  
  if FailedTests > 0 then
    ExitCode := 1;
end.
```

### Running Tests

```bash
# Compile test
fpc -Fusrc -FEtests/bin tests/test_your_feature.pas

# Run test
./tests/bin/test_your_feature
```

---

## 📚 Documentation

### Required Documentation

1. **Code Comments**: For public APIs and complex logic
2. **README Updates**: If adding major features
3. **Examples**: For new functionality
4. **CHANGELOG**: Update with your changes

### Documentation Standards

- Use clear, concise English
- Provide code examples
- Explain *why*, not just *what*
- Keep documentation in sync with code

---

## 🔍 Pull Request Process

### Before Submitting

- [ ] Code compiles without errors
- [ ] All tests pass
- [ ] New tests added for new features
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] No merge conflicts with main branch

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring

## Testing
Describe testing performed

## Checklist
- [ ] Code follows style guidelines
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] No breaking changes (or documented)

## Related Issues
Fixes #123
```

### Review Process

1. **Automated Checks**: CI/CD must pass
2. **Code Review**: At least one maintainer approval required
3. **Testing**: Verify tests pass
4. **Documentation**: Check docs are updated
5. **Merge**: Maintainer will merge when approved

---

## 🐛 Issue Reporting

### Before Creating an Issue

- Search existing issues
- Check documentation
- Try latest version

### Bug Report Template

```markdown
**Describe the bug**
Clear description of what went wrong

**To Reproduce**
1. Step 1
2. Step 2
3. See error

**Expected behavior**
What should happen

**Environment**
- OS: [e.g. Windows 11, Ubuntu 22.04]
- Free Pascal: [e.g. 3.3.1]
- OpenSSL: [e.g. 3.0.8]
- fafafa.ssl: [e.g. v1.0.0]

**Code Sample**
```pascal
// Minimal code to reproduce
```

**Error Message**
```
Full error message here
```

**Additional context**
Any other relevant information
```

### Feature Request Template

```markdown
**Feature Description**
What feature would you like?

**Use Case**
Why is this feature needed?

**Proposed Solution**
How could this be implemented?

**Alternatives**
Other solutions you've considered

**Additional Context**
Any other relevant information
```

---

## 🏅 Recognition

Contributors will be recognized in:
- CHANGELOG.md
- GitHub contributors page
- Project documentation (for significant contributions)

---

## 📞 Communication

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: General questions and discussions
- **Pull Requests**: Code contributions

---

## 📜 License

By contributing, you agree that your contributions will be licensed under the same license as the project (MIT License).

---

## ❓ Questions?

If you have questions about contributing:
1. Check existing documentation
2. Search GitHub Issues and Discussions
3. Open a new Discussion
4. Contact maintainers

---

**Thank you for contributing to fafafa.ssl!** 🎉

---

**Last Updated**: 2025-10-28  
**Version**: 1.0.0

