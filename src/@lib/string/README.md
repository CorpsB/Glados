# LibString

**LibString** is a comprehensive utility library designed for efficient string (`[char]`) manipulation and analysis. It provides a robust set of functions ranging from basic case transformation to advanced cleaning, slicing, and type validation.

## 📋 Project Information

* **Project Name:** LibString
* **File:** `string_utils.npy`
* **Year:** 2026
* **Context:** Epitech Project

## 🚀 Features

### 1. Transformation & Case Handling
Modify the casing and format of your strings.
* `toUpper` / `toLower`: Converts the entire string to uppercase or lowercase.
* `capitalize`: Capitalizes the first character of the string.
* `swapCase`: Swaps the case of every character (upper becomes lower, lower becomes upper).
* `reverse`: Reverses the character order of the string.
* `repeat`: Repeats the string *n* times.

### 2. Cleaning & Sanitization
Remove unwanted characters and format strings for safe usage.
* `trim`, `ltrim`, `rtrim`: Removes whitespace, tabs, and newlines from both ends, the left, or the right.
* `removeSpaces`, `removeTabs`, `removeNewLines`: Strictly removes specific whitespace characters from the entire string.
* `slugify`: Converts a string into a URL-friendly slug (lowercased, special chars removed, spaces replaced by hyphens).

### 3. Slicing, Splitting & Joining
Extract specific parts or restructure your data.
* `split`: Splits a string into a list based on a specific delimiter.
* `lines`: Splits a string into a list of lines.
* `join`: Joins a list of strings into a single string using a delimiter.
* `substring`: Extracts a part of the string starting at an index for a specific length.
* `slice`: Extracts a section of the string from a start index to an end index.
* `left` / `right`: Returns the first or last *n* characters.

### 4. Validation & Analysis
Check the content and properties of your strings.
* `length`: Returns the number of characters.
* `isEmpty`: Checks if the string is effectively empty (`[]`).
* `isBlank`: Checks if the string is empty or contains only whitespace.
* `isAlpha`, `isNumeric`, `isAlnum`: Validates if the string contains only letters, numbers, or both.
* `isUpper` / `isLower`: Checks if the string is strictly uppercase or lowercase.
* `hasOnly`: Verifies if the string consists entirely of a specific character.

### 5. Type Conversion
Convert strings to other primitive types.
* `toInt`: Parses a string to an integer (supports negative numbers).
* `toBool`: Converts the string `"True"` to a boolean value.

---

## 🛠 Usage Examples

### Basic Manipulation
```c
str: [char] = "  Hello World  ";

// Cleaning
clean: [char] = trim(str); // "Hello World"

// Formatting
slug: [char] = slugify(clean); // "hello-world"
reversed: [char] = reverse(clean); // "dlroW olleH"
