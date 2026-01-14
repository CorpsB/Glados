#!/usr/bin/env bash
set -euo pipefail

base="test/Functional/Print"
mkdir -p "$base"

# 1) print(expression)
cat > "$base/expr_ok.npy" <<'EOF'
func main() {
    print(2 + 3 * 4);
}
EOF

# 2) print([int]) with fixed expected format
cat > "$base/list_int_format_ok.npy" <<'EOF'
func main() {
    l: [int] = [1, 2, 3];
    print(l);
}
EOF

# 3) print([[int]]) with fixed expected format
cat > "$base/nested_list_format_ok.npy" <<'EOF'
func main() {
    l: [[int]] = [[1, 2], [3]];
    print(l);
}
EOF

# 4) print(struct) with fixed expected format
cat > "$base/struct_format_ok.npy" <<'EOF'
struct Point {
    x: int;
    y: int;
}

func main() {
    p: Point = new Point { x: 1, y: 2 };
    print(p);
}
EOF

# 5) print(void) must be verifiable:
# We expect print(void) produces truly no output (not even a newline),
# so the only output should be "42".
cat > "$base/void_no_output_ok.npy" <<'EOF'
func v() {
    // does not return -> void
}

func main() {
    print(v());
    print(42);
}
EOF

echo "Generated print-format tests under: $base"
