# CBSTM03A.CBL: Account Statement Generation Program

## Overview
CBSTM03A.CBL is a COBOL batch program designed to generate account statements for a card demo application. It processes transaction data and produces statements in two formats: plain text and HTML. The program reads from multiple input files, including transaction, customer, account, and cross-reference files, to compile comprehensive account statements.

<!-- general-rule-start -->
## General Business Rules:

1. File Processing:
   - The program reads from four main input files: TRNXFILE (transactions), XREFFILE (cross-reference), CUSTFILE (customer information), and ACCTFILE (account details).
   - It writes output to two files: STMT-FILE (plain text statements) and HTML-FILE (HTML formatted statements).

2. Statement Generation Process:
   - For each account, the program:
     a. Retrieves customer information
     b. Fetches account details
     c. Collects all relevant transactions
     d. Generates a statement in both plain text and HTML formats

3. Transaction Handling:
   - Transactions are grouped by card number.
   - The program can handle up to 51 cards with 10 transactions each.
   - Transaction amounts are summed to calculate the total transaction amount for the statement period.

4. Statement Content:
   - Each statement includes:
     - Customer name and address
     - Account ID
     - Current balance
     - FICO credit score
     - Detailed list of transactions (including transaction ID, description, and amount)
     - Total transaction amount for the statement period

5. HTML Formatting:
   - The HTML output includes styled sections for better readability, including:
     - Header with bank information
     - Customer details section
     - Basic account information section
     - Transaction summary table

6. Error Handling:
   - The program checks return codes after file operations and displays error messages if issues occur.
   - In case of critical errors, the program abends using the 'CEE3ABD' call.

7. Performance Considerations:
   - The program uses a two-dimensional array (WS-TRNX-TABLE) to store transaction data in memory, potentially improving performance by reducing file I/O operations.

8. Mainframe Specifics:
   - The program interacts with mainframe control blocks (PSA, TCB, TIOT) to retrieve job and step information.
   - It uses ALTER and GO TO statements for flow control, which are considered obsolete but may be required for legacy system compatibility.

9. Modular Design:
   - The program is structured into separate sections for different functionalities (e.g., file opening, reading, writing, closing), enhancing maintainability.

10. Date Handling:
    - The program doesn't explicitly handle date calculations or formatting, suggesting that date-related operations might be handled externally or by the calling system.
<!-- general-rule-end -->

This program plays a crucial role in generating account statements, which are essential for customer communication and financial record-keeping in the card demo application. It demonstrates complex data processing and formatting capabilities, integrating information from multiple sources to produce comprehensive account statements.
## Dependencies

This program relies on several external files and a subroutine for its operation. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COSTM01 | Copybook containing transaction record layout | Copybook | COSTM01.CPY | May need to be updated if transaction structure changes |
| CVACT03Y | Copybook for card cross-reference entity | Copybook | CVACT03Y.cpy | Important for linking cards to accounts and customers |
| CUSTREC | Copybook for customer record structure | Copybook | CUSTREC.cpy | Contains customer data structure, may need adaptation for modern systems |
| CVACT01Y | Copybook for account entity | Copybook | CVACT01Y.cpy | Account information structure, crucial for statement generation |
| CBSTM03B | Subroutine for file operations | Program | Not provided | File handling logic, may need to be replaced with modern I/O methods |
| TRNXFILE | Transaction data file | File | Not provided | Source of transaction data, format may need updating |
| XREFFILE | Cross-reference data file | File | Not provided | Links cards, accounts, and customers |
| CUSTFILE | Customer data file | File | Not provided | Source of customer information |
| ACCTFILE | Account data file | File | Not provided | Source of account details |
| STMT-FILE | Output file for plain text statements | File | Not provided | May need to be replaced with more modern output formats |
| HTML-FILE | Output file for HTML statements | File | Not provided | HTML structure may need updating to modern standards |

These dependencies are integral to the program's operation, handling data structures, file I/O, and output generation. In a modernization effort, these components would likely need to be reviewed and potentially updated or replaced to align with modern architectures and data handling practices.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the CBSTM03A program. These structures are crucial for processing transactions, customer information, and generating account statements.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | TRNX-RECORD | Group Item | 350 bytes | Transaction record structure | Yes | N/A | Defined in COSTM01 copybook | Yes | Transaction |
| 1.1 | TRNX-KEY | Group Item | 32 bytes | Key for transaction record | Yes | N/A | Composed of card number and transaction ID | Yes | Transaction |
| 1.1.1 | TRNX-CARD-NUM | Alphanumeric | 16 bytes | Card number associated with the transaction | Yes | N/A | Part of the transaction key | Yes | Transaction |
| 1.1.2 | TRNX-ID | Alphanumeric | 16 bytes | Unique identifier for the transaction | Yes | N/A | Part of the transaction key | Yes | Transaction |
| 1.2 | TRNX-TYPE-CD | Alphanumeric | 2 bytes | Transaction type code | Yes | N/A | Identifies the type of transaction | Yes | Transaction |
| 1.3 | TRNX-CAT-CD | Numeric | 4 bytes | Transaction category code | Yes | N/A | Categorizes the transaction | Yes | Transaction |
| 1.4 | TRNX-SOURCE | Alphanumeric | 10 bytes | Source of the transaction | Yes | N/A | Indicates where the transaction originated | Yes | Transaction |
| 1.5 | TRNX-DESC | Alphanumeric | 100 bytes | Description of the transaction | Yes | N/A | Provides details about the transaction | Yes | Transaction |
| 1.6 | TRNX-AMT | Signed Numeric | 11 bytes | Amount of the transaction | Yes | N/A | Includes 2 decimal places | Yes | Transaction |
| 2 | CARD-XREF-RECORD | Group Item | 50 bytes | Card cross-reference record structure | Yes | N/A | Defined in CVACT03Y copybook | Yes | Cross-reference |
| 2.1 | XREF-CARD-NUM | Alphanumeric | 16 bytes | Card number | Yes | N/A | Links to transaction record | Yes | Cross-reference |
| 2.2 | XREF-CUST-ID | Numeric | 9 bytes | Customer ID | Yes | N/A | Links to customer record | Yes | Cross-reference |
| 2.3 | XREF-ACCT-ID | Numeric | 11 bytes | Account ID | Yes | N/A | Links to account record | Yes | Cross-reference |
| 3 | CUSTOMER-RECORD | Group Item | 500 bytes | Customer information structure | Yes | N/A | Defined in CUSTREC copybook | Yes | Customer |
| 3.1 | CUST-ID | Numeric | 9 bytes | Unique customer identifier | Yes | N/A | Primary key for customer record | Yes | Customer |
| 3.2 | CUST-FIRST-NAME | Alphanumeric | 25 bytes | Customer's first name | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.3 | CUST-MIDDLE-NAME | Alphanumeric | 25 bytes | Customer's middle name | No | Spaces | Used in statement generation if present | Yes | Customer |
| 3.4 | CUST-LAST-NAME | Alphanumeric | 25 bytes | Customer's last name | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.5 | CUST-ADDR-LINE-1 | Alphanumeric | 50 bytes | First line of customer's address | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.6 | CUST-ADDR-LINE-2 | Alphanumeric | 50 bytes | Second line of customer's address | No | Spaces | Used in statement generation if present | Yes | Customer |
| 3.7 | CUST-ADDR-LINE-3 | Alphanumeric | 50 bytes | Third line of customer's address | No | Spaces | Used in statement generation if present | Yes | Customer |
| 3.8 | CUST-ADDR-STATE-CD | Alphanumeric | 2 bytes | State code of customer's address | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.9 | CUST-ADDR-COUNTRY-CD | Alphanumeric | 3 bytes | Country code of customer's address | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.10 | CUST-ADDR-ZIP | Alphanumeric | 10 bytes | ZIP code of customer's address | Yes | N/A | Used in statement generation | Yes | Customer |
| 3.11 | CUST-FICO-CREDIT-SCORE | Numeric | 3 bytes | Customer's FICO credit score | Yes | N/A | Used in statement generation | Yes | Customer |
| 4 | ACCOUNT-RECORD | Group Item | 300 bytes | Account information structure | Yes | N/A | Defined in CVACT01Y copybook | Yes | Account |
| 4.1 | ACCT-ID | Numeric | 11 bytes | Unique account identifier | Yes | N/A | Primary key for account record | Yes | Account |
| 4.2 | ACCT-CURR-BAL | Signed Numeric | 12 bytes | Current balance of the account | Yes | N/A | Includes 2 decimal places | Yes | Account |
| 4.3 | ACCT-CREDIT-LIMIT | Signed Numeric | 12 bytes | Credit limit of the account | Yes | N/A | Includes 2 decimal places | Yes | Account |
| 5 | WS-TRNX-TABLE | Group Item | Variable | In-memory transaction table | Yes | N/A | Used for efficient transaction processing | Yes | Working Storage |
| 5.1 | WS-CARD-TBL | Group Item | Occurs 51 times | Array of card entries | Yes | N/A | Stores transactions grouped by card | Yes | Working Storage |
| 5.1.1 | WS-CARD-NUM | Alphanumeric | 16 bytes | Card number | Yes | N/A | Identifies the card for transactions | Yes | Working Storage |
| 5.1.2 | WS-TRAN-TBL | Group Item | Occurs 10 times | Array of transactions for each card | Yes | N/A | Stores individual transactions | Yes | Working Storage |
| 5.1.2.1 | WS-TRAN-NUM | Alphanumeric | 16 bytes | Transaction ID | Yes | N/A | Identifies individual transactions | Yes | Working Storage |
| 5.1.2.2 | WS-TRAN-REST | Alphanumeric | 318 bytes | Remaining transaction details | Yes | N/A | Stores other transaction information | Yes | Working Storage |

This data structure is crucial for the program's operation, handling various aspects of transaction processing and statement generation. In a modernization effort, these structures would likely need to be adapted to more modern data formats or database schemas, while preserving the essential information and relationships between different entities.



<!--rule-start-->
## Reviewed Rule 1
- ## Rule / Function / Method - Name: Initialize and Open Files

- Detailed Description of the Rule:
  This rule initializes the program by opening all necessary input and output files, checking the Job Control Language (JCL) information, displaying it, and preparing the transaction table for processing.

- What it proposes to do:
  Ensure all required files are open and ready for processing, verify the correct JCL environment, and set up initial data structures for the program.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Access the Program Status Area (PSA) block using the PSAPTR.
2. Retrieve the Task Control Block (TCB) address from the PSA.
3. Get the Task Input/Output Table (TIOT) address from the TCB.
4. Display the JCL information (Job Name and Step Name) from the TIOT.
5. Traverse the TIOT to display all DD names and their UCB (Unit Control Block) status.
6. Open the output files:
   a. Open STMT-FILE for writing plain text statements.
   b. Open HTML-FILE for writing HTML formatted statements.
7. Initialize the transaction table (WS-TRNX-TABLE) and its counter (WS-TRN-TBL-CNTR).
8. Open the input files in the following order:
   a. Open TRNXFILE (Transaction file)
   b. Open XREFFILE (Cross-reference file)
   c. Open CUSTFILE (Customer file)
   d. Open ACCTFILE (Account file)
9. For each file opening:
   a. Set the file name in WS-M03B-DD.
   b. Set M03B-OPEN to TRUE.
   c. Call 'CBSTM03B' with WS-M03B-AREA.
   d. Check the return code (WS-M03B-RC):
      - If '00' or '04', continue.
      - Otherwise, display an error message and abort the program.
10. After opening TRNXFILE:
    a. Read the first record from TRNXFILE.
    b. Initialize CR-CNT to 1 and TR-CNT to 0.
    c. Store the first card number in WS-SAVE-CARD.
11. Set WS-FL-DD to 'READTRNX' to prepare for transaction processing.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - Name: Process Transactions

- Detailed Description of the Rule:
  This rule reads all transactions from TRNXFILE and organizes them into a two-dimensional array for efficient processing. It also handles the opening and closing of multiple files, and creates statements in both plain text and HTML formats.

- What it proposes to do:
  Create an in-memory structure of transactions grouped by card number for quick access during statement generation, process customer and account information, and generate detailed statements.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize CR-CNT (card counter) to 1 and TR-CNT (transaction counter) to 0.
2. Open TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE, STMT-FILE, and HTML-FILE.
3. Read the first transaction record from TRNXFILE.
4. Store the first card number in WS-SAVE-CARD.
5. For each transaction record:
   a. If the current transaction's card number (TRNX-CARD-NUM) matches WS-SAVE-CARD:
      - Increment TR-CNT by 1
   b. Else:
      - Store TR-CNT in WS-TRCT(CR-CNT)
      - Increment CR-CNT by 1
      - Reset TR-CNT to 1
   c. Store the transaction data:
      - TRNX-CARD-NUM to WS-CARD-NUM(CR-CNT)
      - TRNX-ID to WS-TRAN-NUM(CR-CNT, TR-CNT)
      - TRNX-REST to WS-TRAN-REST(CR-CNT, TR-CNT)
   d. Update WS-SAVE-CARD with the current TRNX-CARD-NUM
   e. Read the next transaction record
6. Repeat step 5 until end-of-file is reached for TRNXFILE.
7. After reaching end-of-file:
   - Store the final TR-CNT in WS-TRCT(CR-CNT)
8. Process XREFFILE records:
   a. For each XREFFILE record:
      - Retrieve corresponding CUSTFILE and ACCTFILE records
      - Generate statement in plain text format
      - Generate statement in HTML format
      - Process transactions for the current card:
        * Iterate through the transaction array
        * Write transaction details to statement
        * Calculate and write total transaction amount
9. Close all opened files (TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE, STMT-FILE, HTML-FILE).

## Additional Details:
- The program uses a subroutine call (CALL 'CBSTM03B') for file operations.
- Error handling is implemented for file operations, with appropriate error messages and program termination.
- The program generates both plain text and HTML formatted statements.
- The two-dimensional array (WS-TRNX-TABLE) is used to store transactions, with WS-CARD-TBL occurring 51 times and WS-TRAN-TBL occurring 10 times for each card.
- The program uses ALTER and GO TO statements for control flow, which may need to be refactored in modernization.
- The program accesses system control blocks (PSA, TCB, TIOT) for JCL information.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
- ## Rule / Function / Method - Name: Generate Statements

- Detailed Description of the Rule:
  This rule processes each account, retrieves relevant information, and generates both plain text and HTML statements.

- What it proposes to do:
  Create comprehensive account statements for each customer, including transaction details and account summary.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open output files for plain text (STMT-FILE) and HTML (HTML-FILE) statements.
2. Initialize transaction table (WS-TRNX-TABLE) and counters.
3. Open and read TRNXFILE to populate the transaction table.
4. Read records from XREFFILE (cross-reference file) until end-of-file.
5. For each XREFFILE record:
   a. Retrieve customer information:
      - Use XREF-CUST-ID as the key to read from CUSTFILE
   b. Retrieve account information:
      - Use XREF-ACCT-ID as the key to read from ACCTFILE
   c. Generate the statement header:
      - Combine customer name (CUST-FIRST-NAME, CUST-MIDDLE-NAME, CUST-LAST-NAME)
      - Add address lines (CUST-ADDR-LINE-1, CUST-ADDR-LINE-2, CUST-ADDR-LINE-3)
      - Include state (CUST-ADDR-STATE-CD), country (CUST-ADDR-COUNTRY-CD), and ZIP code (CUST-ADDR-ZIP)
   d. Add account details:
      - Account ID (ACCT-ID)
      - Current Balance (ACCT-CURR-BAL)
      - FICO Credit Score (CUST-FICO-CREDIT-SCORE)
   e. Process transactions:
      - Initialize WS-TOTAL-AMT to zero
      - Loop through the transaction table (WS-TRNX-TABLE):
        - For each card that matches XREF-CARD-NUM:
          - Write transaction details (ID, description, amount) to the statement
          - Add TRNX-AMT to WS-TOTAL-AMT
   f. Write the total transaction amount (WS-TOTAL-AMT) to the statement
   g. Write the statement to both STMT-FILE (plain text) and HTML-FILE (HTML format)
6. Close all files (TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE, STMT-FILE, HTML-FILE).

## Additional Details:
- The program uses a subroutine (CBSTM03B) for file operations (open, close, read, write).
- Error handling is implemented for file operations, with appropriate error messages and program termination.
- The HTML statement includes styling and formatting for better readability.
- The program uses COMP and COMP-3 variables for efficient numeric processing.
- A two-dimensional array (WS-TRNX-TABLE) is used to store transaction data in memory.
- The program checks and displays DD names from the TIOT (Task Input/Output Table) at the beginning of execution.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: Format HTML Statement

- Detailed Description of the Rule:
  This rule generates an HTML-formatted statement with proper structure and styling, including specific details for customer information, account details, and transaction data.

- What it proposes to do:
  Create a visually appealing and structured HTML document for each account statement, including bank information, customer details, account summary, and a detailed transaction list.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Write HTML header:
   - <!DOCTYPE html>
   - <html lang="en">
   - <head> with appropriate meta tags and title
2. Create the main table structure:
   - Use a centered table with 70% width and specific font styling (12px Segoe UI,sans-serif)
3. Add bank information section:
   - Include bank name (Bank of XYZ), address (410 Terry Ave N, Seattle WA 99999), and styling with background color (#1d1d96b3)
4. Create customer information section:
   - Add customer name (combining first, middle, and last name) and full address with appropriate styling (background color: #f2f2f2)
5. Generate basic account details section:
   - Account ID
   - Current Balance
   - FICO Score
6. Create transaction summary section:
   - Add a header for the transaction summary (background color: #33FFD1)
   - Create a table for transactions with columns:
     - Transaction ID (width: 25%, background color: #33FF5E)
     - Transaction Details (width: 55%, background color: #33FF5E)
     - Amount (width: 20%, background color: #33FF5E, right-aligned)
7. For each transaction:
   - Create a new table row
   - Add transaction ID in the first column
   - Add transaction details in the second column
   - Add transaction amount in the third column, right-aligned
8. After all transactions:
   - Add a row for the total transaction amount
9. Add an "End of Statement" header
10. Close all open HTML tags and structure elements
11. Write the complete HTML content to HTML-FILE
12. Specific styling details:
    - Use background color #FFAF33 for the bank information section
    - Use background color #f2f2f2 for customer information and transaction rows
    - Center-align the "Basic Details" and "Transaction Summary" headers
    - Use 16px font size for specific elements (bank name, section headers, column headers)
13. Error handling:
    - Check return codes after each file operation (open, read, write, close)
    - Display error messages and abort the program if any operation fails

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: Close Files and Terminate

- Detailed Description of the Rule:
  This rule ensures proper closure of all opened files and terminates the program. It handles the closure of TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE, STMT-FILE, and HTML-FILE, checking for errors during the process.

- What it proposes to do:
  Safely close all file connections, handle any errors that occur during file closure, and end the program execution.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Close TRNXFILE:
   - Set WS-M03B-DD to 'TRNXFILE'
   - Set M03B-CLOSE to TRUE
   - Set WS-M03B-RC to ZERO
   - Call 'CBSTM03B' with WS-M03B-AREA
   - Check return code (WS-M03B-RC):
     - If '00' or '04', continue
     - Otherwise, display error message "ERROR CLOSING TRNXFILE" and return code, then perform 9999-ABEND-PROGRAM
2. Close XREFFILE:
   - Repeat steps from 1, replacing 'TRNXFILE' with 'XREFFILE'
3. Close CUSTFILE:
   - Repeat steps from 1, replacing 'TRNXFILE' with 'CUSTFILE'
4. Close ACCTFILE:
   - Repeat steps from 1, replacing 'TRNXFILE' with 'ACCTFILE'
5. Close STMT-FILE using COBOL file handling (CLOSE statement)
6. Close HTML-FILE using COBOL file handling (CLOSE statement)
7. If any errors occurred during file closing (as checked in steps 1-4):
   - The program will have already abended in the respective closing procedure
8. If all files closed successfully:
   - Execute GOBACK to terminate the program

Note: The 9999-ABEND-PROGRAM procedure displays 'ABENDING PROGRAM' and calls 'CEE3ABD' to abnormally terminate the program if any errors occur during file closure.
```

<!--rule-end-->