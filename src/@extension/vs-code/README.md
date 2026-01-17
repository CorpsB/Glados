# Noupy Language Support (VS Code)

This extension provides:

- Syntax highlighting (TextMate grammar)
- Basic language configuration (comments, brackets, auto-closing)
- Starter snippets

## File association

Noupy uses the `.npy` extension. If another extension claims `.npy` on your machine, set:

```json
"files.associations": {
  "*.npy": "noupy"
}
```

## Development

- Open this folder in VS Code
- Press `F5` to launch an Extension Development Host
- Open a `.npy` file and verify highlighting

Generated: 2026-01-13
