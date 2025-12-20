# Contributing to fafafa.ssl

Thank you for your interest in contributing to **fafafa.ssl**! 🎉

## 🌟 How to Contribute

### Reporting Bugs 🐛

1. **Check existing issues** first to avoid duplicates
2. **Use the bug report template**:
   - Environment (OS, FPC version, OpenSSL version)
   - Steps to reproduce
   - Expected vs actual behavior
   - Error messages/logs
3. **Minimal reproduction case** if possible

### Suggesting Features ✨

1. **Open a feature request** issue
2. **Describe the use case** and why it's valuable
3. **Propose an API** if applicable
4. **Consider backwards compatibility**

### Submitting Pull Requests 🚀

#### Before You Start
- [ ] Discuss major changes in an issue first
- [ ] Check the roadmap in `implementation_plan.md`
- [ ] Ensure your change fits the project scope

#### Development Process

1. **Fork** the repository
2. **Create a feature branch**:
   ```bash
   git checkout -b feature/my-awesome-feature
   ```

3. **Make your changes**:
   - Follow existing code style
   - Add tests for new functionality
   - Update documentation

4. **Test thoroughly**:
   ```bash
   ./ci_pipeline.sh all
   ```

5. **Commit with clear messages**:
   ```bash
   git commit -m "Add AES-128-CBC support"
   ```

6. **Push and create PR**:
   ```bash
   git push origin feature/my-awesome-feature
   ```

## 📝 Code Style Guidelines

### Pascal Conventions
- **Indentation**: 2 spaces (no tabs)
- **Naming**:
  - Classes: `TPascalCase`
  - Interfaces: `IInterfaceName`
  - Variables: `camelCase` for locals, `FFieldName` for fields
  - Constants: `UPPER_CASE`
- **Comments**: Clear, concise, above the code

### Example
```pascal
type
  { Brief class description }
  TMyClass = class
  private
    FFieldName: Integer;
    procedure PrivateMethod;
  public
    { Public method description
      @param AParameter Description
      @return Description }
    function PublicMethod(AParameter: string): Boolean;
  end;
```

## 🧪 Testing Requirements

All contributions must include appropriate tests:

1. **Unit Tests**: For new utility functions
2. **Integration Tests**: For TLS/crypto operations
3. **Benchmarks**: If performance-critical

Run tests before submitting:
```bash
./ci_pipeline.sh test
./ci_pipeline.sh bench
```

## 🔒 Security Guidelines

- **Never commit** credentials, API keys, or certificates
- **Use secure defaults** (TLS 1.2+, strong ciphers)
- **Validate all inputs** before processing
- **Clear sensitive data** from memory after use
- **Follow OWASP** guidelines for cryptography

## 📋 Pull Request Checklist

- [ ] Code follows project style
- [ ] All tests pass (`./ci_pipeline.sh all`)
- [ ] New tests added for new features
- [ ] Documentation updated (README, code comments)
- [ ] No performance regression (check benchmarks)
- [ ] commit message is clear and descriptive
- [ ] Branch is up-to-date with main

## 🏗️ Project Structure

```
fafafa.ssl/
├── src/                 # Core library code
│   ├── fafafa.ssl.*.pas    # Main modules
│   └── openssl/         # OpenSSL backend
├── examples/            # Example programs
├── tests/               # Test suites
│   ├── unit/           # Unit tests
│   └── benchmarks/     # Performance tests
└── docs/                # Documentation
```

## 💬 Communication

- **Issues**: For bugs, features, questions
- **Pull Requests**: For code contributions
- **Discussions**: For general topics, ideas

## 🎖️ Recognition

Contributors will be acknowledged in:
- `CONTRIBUTORS.md` file
- Release notes
- Project documentation

## 📄 License

By contributing, you agree that your contributions will be licensed under the same license as the project (MIT).

---

**Thank you for making fafafa.ssl better! 🙏**
