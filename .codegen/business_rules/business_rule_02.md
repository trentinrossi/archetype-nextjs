# CBSTM03B.CBL: Transaction Report File Processing Subroutine

## Overview
This COBOL program, CBSTM03B.CBL, is a batch subroutine designed to handle file processing operations related to the Transaction Report functionality in the CardDemo application. It serves as a crucial component in managing various data files essential for generating transaction reports.

The program's main purpose is to provide file handling capabilities for four different types of files:
1. Transaction File (TRNXFILE)
2. Cross-Reference File (XREFFILE)
3. Customer File (CUSTFILE)
4. Account File (ACCTFILE)

It supports operations such as opening, reading, and closing these files, allowing the main program to access and manipulate the necessary data for transaction reporting.

<!-- general-rule-start -->
## General Business Rules:

1. File Handling Operations:
   - The program supports five types of operations for each file:
     a. Open (O): Opens the file for processing
     b. Close (C): Closes the file after processing
     c. Read (R): Reads a record from the file sequentially
     d. Read Key (K): Reads a record from the file using a specific key (for CUSTFILE and ACCTFILE only)
     e. Write (W): Writes a record to the file (not implemented in this version)
     f. Rewrite (Z): Rewrites a record in the file (not implemented in this version)

2. File Structures:
   - TRNXFILE (Transaction File):
     - Indexed organization with sequential access
     - Key: FD-TRNXS-ID (combination of card number and transaction ID)
   - XREFFILE (Cross-Reference File):
     - Indexed organization with sequential access
     - Key: FD-XREF-CARD-NUM (card number)
   - CUSTFILE (Customer File):
     - Indexed organization with random access
     - Key: FD-CUST-ID (customer ID)
   - ACCTFILE (Account File):
     - Indexed organization with random access
     - Key: FD-ACCT-ID (account ID)

3. Operation Flow:
   - The program receives input parameters through the LK-M03B-AREA linkage section.
   - It determines which file to process based on the LK-M03B-DD field.
   - The specific operation to perform is determined by the LK-M03B-OPER field.
   - After performing the requested operation, the program returns the file status in LK-M03B-RC.

4. Error Handling:
   - File status codes are captured for each operation and returned to the calling program.
   - The program does not include explicit error handling logic; it's assumed that the calling program will interpret the returned status codes.

5. Data Transfer:
   - For read operations, the program moves the read data into the LK-M03B-FLDT field in the linkage section.
   - For key-based reads (CUSTFILE and ACCTFILE), the key is provided in LK-M03B-KEY with its length in LK-M03B-KEY-LN.

6. Modularity:
   - The program is designed to be called by other programs, likely the main transaction report generation program.
   - It encapsulates all file I/O operations, promoting code reusability and maintainability.

7. Limitations:
   - The program currently does not support write or rewrite operations, although placeholders for these operations exist in the code structure.
   - There's no explicit handling for end-of-file conditions or other specific error scenarios.

This subroutine plays a critical role in the transaction reporting process by managing the access to various data files. It allows the main program to retrieve transaction details, cross-reference information, customer data, and account information efficiently, which are essential for generating comprehensive transaction reports.
<!-- general-rule-end -->
## Dependencies

This program, CBSTM03B.CBL, is a subroutine that handles file operations for the transaction reporting functionality. It relies on several external files for its operations. These dependencies are crucial for the program's functionality and would need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| TRNXFILE | Transaction file containing transaction records | Indexed File | TRNXFILE | High - Contains core transaction data; may need to be migrated to a modern database system |
| XREFFILE | Cross-reference file linking cards to accounts and customers | Indexed File | XREFFILE | High - Essential for relating transactions to customers; consider integrating into a relational database |
| CUSTFILE | Customer file containing customer information | Indexed File | CUSTFILE | High - Contains customer data; prime candidate for migration to a customer management system or database |
| ACCTFILE | Account file containing account details | Indexed File | ACCTFILE | High - Holds account information; consider integrating with a modern banking system or database |
| COSTM01 | Copybook for transaction record layout | Copybook | COSTM01.CPY | Medium - Defines data structure; may need to be converted to a modern data schema |
| CVACT03Y | Copybook for card cross-reference record layout | Copybook | CVACT03Y.CPY | Medium - Defines data structure; consider integrating into a relational database schema |
| CVCUS01Y | Copybook for customer record layout | Copybook | CVCUS01Y.CPY | Medium - Defines customer data structure; important for data migration |
| CVACT01Y | Copybook for account record layout | Copybook | CVACT01Y.CPY | Medium - Defines account data structure; crucial for modernizing account management |

These dependencies represent the core data structures and files that the program interacts with. In a modernization effort, these indexed files would likely be replaced by a relational database or a more modern data storage solution. The copybooks define the data structures and would be crucial in mapping the existing data to new schemas or object models in a modernized system.
## Detailed Rules
## Data Structure

This section details the data structures used in the CBSTM03B.CBL program, focusing on the linkage section and file definitions. These structures are crucial for understanding the data flow and file operations in the program.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| LK-M03B-DD | File Identifier | Alphanumeric | 8 | Identifies which file to operate on | Yes | None | Used to select the appropriate file procedure | High | Control |
| LK-M03B-OPER | Operation Code | Alphanumeric | 1 | Specifies the operation to perform | Yes | None | 'O' for Open, 'C' for Close, 'R' for Read, 'K' for Read by Key | High | Control |
| LK-M03B-RC | Return Code | Alphanumeric | 2 | Stores the file status after an operation | Yes | None | Used for error handling by the calling program | High | Status |
| LK-M03B-KEY | Key Value | Alphanumeric | 25 | Key for read operations | No | None | Used for key-based reads on CUSTFILE and ACCTFILE | High | Data |
| LK-M03B-KEY-LN | Key Length | Numeric | 4 | Length of the key value | No | None | Specifies how many characters of LK-M03B-KEY to use | Medium | Control |
| LK-M03B-FLDT | File Data | Alphanumeric | 1000 | Buffer for read/write operations | Yes | None | Holds the entire record read from or written to a file | High | Data |
| FD-TRNXS-ID | Transaction ID | Alphanumeric | 32 | Composite key for TRNXFILE | Yes | None | Consists of 16-char card number and 16-char transaction ID | High | Key |
| FD-XREF-CARD-NUM | Card Number | Alphanumeric | 16 | Key for XREFFILE | Yes | None | Used to link transactions to accounts and customers | High | Key |
| FD-CUST-ID | Customer ID | Alphanumeric | 9 | Key for CUSTFILE | Yes | None | Unique identifier for customers | High | Key |
| FD-ACCT-ID | Account ID | Numeric | 11 | Key for ACCTFILE | Yes | None | Unique identifier for accounts | High | Key |
| TRNXFILE-STATUS | Transaction File Status | Alphanumeric | 2 | Status of TRNXFILE operations | Yes | None | Used for error handling | Medium | Status |
| XREFFILE-STATUS | Cross-Reference File Status | Alphanumeric | 2 | Status of XREFFILE operations | Yes | None | Used for error handling | Medium | Status |
| CUSTFILE-STATUS | Customer File Status | Alphanumeric | 2 | Status of CUSTFILE operations | Yes | None | Used for error handling | Medium | Status |
| ACCTFILE-STATUS | Account File Status | Alphanumeric | 2 | Status of ACCTFILE operations | Yes | None | Used for error handling | Medium | Status |



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - File Operation Handler

- Detailed Description of the Rule:
  This rule handles various file operations for four different file types: TRNXFILE, XREFFILE, CUSTFILE, and ACCTFILE. It provides a unified interface for opening, closing, reading, and potentially writing to these files.

- What it proposes to do:
  The rule aims to encapsulate all file I/O operations, providing a centralized point for file handling in the transaction reporting process. It allows the calling program to perform operations on different files without needing to know the specifics of each file's structure or access method.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Receive input parameters through LK-M03B-AREA:
   - LK-M03B-DD: File identifier (TRNXFILE, XREFFILE, CUSTFILE, or ACCTFILE)
   - LK-M03B-OPER: Operation to perform (O: Open, C: Close, R: Read, K: Read by Key)
   - LK-M03B-KEY: Key for key-based operations
   - LK-M03B-KEY-LN: Length of the key

2. Based on LK-M03B-DD, determine which file-specific procedure to execute:
   - TRNXFILE: Go to 1000-TRNXFILE-PROC
   - XREFFILE: Go to 2000-XREFFILE-PROC
   - CUSTFILE: Go to 3000-CUSTFILE-PROC
   - ACCTFILE: Go to 4000-ACCTFILE-PROC
   - If none of the above, go to 9999-GOBACK and exit the program

3. For each file-specific procedure:
   a. If LK-M03B-OPER is 'O' (Open):
      - Open the file in INPUT mode
   b. If LK-M03B-OPER is 'R' (Read) for TRNXFILE and XREFFILE:
      - Perform a sequential read operation
      - Move the read data into LK-M03B-FLDT
   c. If LK-M03B-OPER is 'K' (Read by Key) for CUSTFILE and ACCTFILE:
      - Move LK-M03B-KEY (1:LK-M03B-KEY-LN) to the file's key field (FD-CUST-ID or FD-ACCT-ID)
      - Perform a random read operation using the key
      - Move the read data into LK-M03B-FLDT
   d. If LK-M03B-OPER is 'C' (Close):
      - Close the file

4. After performing the operation:
   - Move the file status (TRNXFILE-STATUS, XREFFILE-STATUS, CUSTFILE-STATUS, or ACCTFILE-STATUS) to LK-M03B-RC

5. Return control to the calling program

Note: The current implementation does not include logic for 'W' (Write) or 'Z' (Rewrite) operations, although these values are defined in the LINKAGE SECTION.

Validation Rules:
- The program does not explicitly validate the input parameters. It's assumed that the calling program provides valid inputs.
- File status codes are returned to the calling program, which is expected to handle any error conditions.

Data Handling:
- For read operations, data is moved from the file record to LK-M03B-FLDT in the linkage section.
- For key-based reads (CUSTFILE and ACCTFILE), the key is provided in LK-M03B-KEY and its length in LK-M03B-KEY-LN.
- TRNXFILE and XREFFILE use sequential access mode, while CUSTFILE and ACCTFILE use random access mode.

Error Handling:
- File status codes are captured for each operation and returned in LK-M03B-RC.
- The program does not include explicit error handling logic; it's assumed that the calling program will interpret the returned status codes.

File Structures:
- TRNXFILE: Key is FD-TRNXS-ID (32 characters), composed of FD-TRNX-CARD (16 characters) and FD-TRNX-ID (16 characters)
- XREFFILE: Key is FD-XREF-CARD-NUM (16 characters)
- CUSTFILE: Key is FD-CUST-ID (9 characters)
- ACCTFILE: Key is FD-ACCT-ID (11 digits)
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - File Processing

- Detailed Description of the Rule:
  This rule handles operations on multiple files: TRNXFILE (transaction records), XREFFILE (cross-reference records), CUSTFILE (customer records), and ACCTFILE (account records).

- What it proposes to do:
  It provides functionality to open, read, and close these files based on the input parameters.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. When called, evaluate LK-M03B-DD to determine which file to process:
   - If 'TRNXFILE', perform TRNXFILE processing
   - If 'XREFFILE', perform XREFFILE processing
   - If 'CUSTFILE', perform CUSTFILE processing
   - If 'ACCTFILE', perform ACCTFILE processing
   - If none of the above, return control to the calling program

2. For TRNXFILE processing:
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT TRNX-FILE
   b. If LK-M03B-OPER is 'R' (Read):
      - Execute READ TRNX-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE TRNX-FILE
   d. Move TRNXFILE-STATUS to LK-M03B-RC

3. For XREFFILE processing:
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT XREF-FILE
   b. If LK-M03B-OPER is 'R' (Read):
      - Execute READ XREF-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE XREF-FILE
   d. Move XREFFILE-STATUS to LK-M03B-RC

4. For CUSTFILE processing:
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT CUST-FILE
   b. If LK-M03B-OPER is 'K' (Read with Key):
      - Move LK-M03B-KEY (1:LK-M03B-KEY-LN) to FD-CUST-ID
      - Execute READ CUST-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE CUST-FILE
   d. Move CUSTFILE-STATUS to LK-M03B-RC

5. For ACCTFILE processing:
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT ACCT-FILE
   b. If LK-M03B-OPER is 'K' (Read with Key):
      - Move LK-M03B-KEY (1:LK-M03B-KEY-LN) to FD-ACCT-ID
      - Execute READ ACCT-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE ACCT-FILE
   d. Move ACCTFILE-STATUS to LK-M03B-RC

6. Return control to the main procedure

Data Handling:
- For read operations, the entire record is moved into LK-M03B-FLDT.
- The record structures are defined as follows:
  - TRNXFILE: FD-TRNXFILE-REC (FD-TRNXS-ID: 32 characters, FD-ACCT-DATA: 318 characters)
  - XREFFILE: FD-XREFFILE-REC (FD-XREF-CARD-NUM: 16 characters, FD-XREF-DATA: 34 characters)
  - CUSTFILE: FD-CUSTFILE-REC (FD-CUST-ID: 9 characters, FD-CUST-DATA: 491 characters)
  - ACCTFILE: FD-ACCTFILE-REC (FD-ACCT-ID: 11 digits, FD-ACCT-DATA: 289 characters)

Error Handling:
- The file status for each operation is captured in the respective file status variable (TRNXFILE-STATUS, XREFFILE-STATUS, CUSTFILE-STATUS, ACCTFILE-STATUS) and returned to the calling program in LK-M03B-RC.
- No explicit error handling is performed within this procedure.

File Access Methods:
- TRNXFILE: Indexed, Sequential access
- XREFFILE: Indexed, Sequential access
- CUSTFILE: Indexed, Random access
- ACCTFILE: Indexed, Random access
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
- ## Rule / Function / Method - XREFFILE Processing

- Detailed Description of the Rule:
  This rule manages operations on the XREFFILE, which contains cross-reference information linking cards to accounts and customers.

- What it proposes to do:
  It provides functionality to open, read sequentially, and close the XREFFILE.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. When called with LK-M03B-DD set to 'XREFFILE':
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT XREF-FILE
   b. If LK-M03B-OPER is 'R' (Read):
      - Execute READ XREF-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE XREF-FILE

2. After performing the operation:
   - Move XREFFILE-STATUS to LK-M03B-RC

3. Return control to the main procedure

Data Handling:
- For read operations, the entire record is moved into LK-M03B-FLDT.
- The record structure is defined by FD-XREFFILE-REC, which includes:
  - FD-XREF-CARD-NUM (16 characters for card number)
  - FD-XREF-DATA (34 characters of additional cross-reference data)

File Organization:
- XREF-FILE is an indexed file with sequential access mode.
- The record key is FD-XREF-CARD-NUM.

Error Handling:
- The file status is captured in XREFFILE-STATUS and returned to the calling program via LK-M03B-RC.
- No explicit error handling is performed within this procedure.

Additional Notes:
- This procedure is part of a larger program (CBSTM03B) that handles multiple file operations.
- The XREFFILE processing is one of four file processing routines in the program, alongside TRNXFILE, CUSTFILE, and ACCTFILE.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - CUSTFILE Processing

- Detailed Description of the Rule:
  This rule handles operations on the CUSTFILE, which contains customer information.

- What it proposes to do:
  It provides functionality to open, read by key, and close the CUSTFILE.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. When called with LK-M03B-DD set to 'CUSTFILE':
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT CUST-FILE
   b. If LK-M03B-OPER is 'K' (Read by Key):
      - Move LK-M03B-KEY (1:LK-M03B-KEY-LN) to FD-CUST-ID
      - Execute READ CUST-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE CUST-FILE

2. After performing the operation:
   - Move CUSTFILE-STATUS to LK-M03B-RC

3. Return control to the main procedure

Data Handling:
- For read operations, the entire record is moved into LK-M03B-FLDT.
- The record structure is defined by FD-CUSTFILE-REC, which includes:
  - FD-CUST-ID (9 characters for customer ID)
  - FD-CUST-DATA (491 characters of additional customer data)

Key Handling:
- For key-based reads, the key is taken from LK-M03B-KEY, using the length specified in LK-M03B-KEY-LN.
- The CUST-FILE is organized as an INDEXED file with RANDOM access mode.

Error Handling:
- The file status is captured in CUSTFILE-STATUS and returned to the calling program in LK-M03B-RC.
- No explicit error handling is performed within this procedure.

Additional Details:
- This CUSTFILE processing is part of a larger program (CBSTM03B) that handles multiple file operations.
- The program uses a common interface (LK-M03B-AREA) for all file operations, with specific fields for operation type, key, and data transfer.
- The CUSTFILE operation is selected when LK-M03B-DD is set to 'CUSTFILE'.
- After completing the CUSTFILE operation, control is returned to the main procedure (0000-START) which may process other file operations.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
- ## Rule / Function / Method - ACCTFILE Processing

- Detailed Description of the Rule:
  This rule manages operations on the ACCTFILE, which contains account information.

- What it proposes to do:
  It provides functionality to open, read by key, and close the ACCTFILE.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. When called with LK-M03B-DD set to 'ACCTFILE':
   a. If LK-M03B-OPER is 'O' (Open):
      - Execute OPEN INPUT ACCT-FILE
   b. If LK-M03B-OPER is 'K' (Read by Key):
      - Move LK-M03B-KEY (1:LK-M03B-KEY-LN) to FD-ACCT-ID
      - Execute READ ACCT-FILE INTO LK-M03B-FLDT
   c. If LK-M03B-OPER is 'C' (Close):
      - Execute CLOSE ACCT-FILE

2. After performing the operation:
   - Move ACCTFILE-STATUS to LK-M03B-RC

3. Return control to the main procedure

Data Handling:
- For read operations, the entire record is moved into LK-M03B-FLDT.
- The record structure is defined by FD-ACCTFILE-REC, which includes:
  - FD-ACCT-ID (11 digit numeric for account ID)
  - FD-ACCT-DATA (289 characters of additional account data)

Key Handling:
- For key-based reads, the key is taken from LK-M03B-KEY, using the length specified in LK-M03B-KEY-LN.
- The key (FD-ACCT-ID) is defined as a numeric field with 11 digits.

File Organization:
- ACCT-FILE is an indexed file with random access mode.
- The record key is FD-ACCT-ID.

Error Handling:
- The file status is captured in ACCTFILE-STATUS and returned to the calling program in LK-M03B-RC.
- ACCTFILE-STATUS consists of two characters (ACCTFILE-STAT1 and ACCTFILE-STAT2).
- No explicit error handling is performed within this procedure.

Additional Notes:
- This procedure is part of a larger program (CBSTM03B) that handles multiple file operations.
- The program uses 88-level condition names for operation types (e.g., M03B-OPEN, M03B-READ-K, M03B-CLOSE).
- The ACCTFILE processing is performed in the 4000-ACCTFILE-PROC section of the program.

<!--rule-end-->