# CBTRN03C.CBL: Transaction Detail Report Generator

## Overview
This COBOL program, CBTRN03C.CBL, is part of the CardDemo application. It is a batch program designed to generate a detailed transaction report. The program reads transaction records, cross-references them with account and card information, and produces a formatted report of transaction details within a specified date range.

<!-- general-rule-start -->
## General Business Rules:
1. The program processes transaction records from an input file (TRANFILE).
2. It filters transactions based on a date range specified in a parameter file (DATEPARM).
3. The program cross-references transaction data with card and account information from an indexed file (CARDXREF).
4. Transaction types and categories are looked up in separate files (TRANTYPE and TRANCATG) to provide descriptive information in the report.
5. The report is generated with the following key features:
   - Transactions are grouped by card number.
   - Page totals, account totals, and grand totals are calculated and displayed.
   - The report includes headers and detailed transaction information.
6. The program handles various file operations (open, read, write, close) for multiple input and output files.
7. Error handling is implemented for file operations, with specific error messages and program termination in case of critical errors.

Key processing steps include:
- Opening all required files (transaction, report, cross-reference, transaction type, transaction category, and date parameter files).
- Reading the date range parameters.
- Processing transactions within the specified date range.
- Looking up additional information for each transaction (account details, transaction type, and category).
- Writing detailed transaction information to the report file.
- Calculating and writing totals (page, account, and grand totals).
- Closing all files upon completion.

The program ensures data integrity by performing validity checks on key fields and aborting the process if critical errors are encountered.
<!-- general-rule-end -->

This program plays a crucial role in providing detailed transaction insights, which can be used for auditing, customer service, and financial reporting purposes within the credit card processing system.
## Dependencies

This program relies on several external files for input and output operations. These dependencies are crucial for the program's functionality, as they provide transaction data, reference information, and parameters, as well as serve as the output for the generated report.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| TRANFILE | Input file containing transaction records | Sequential File | TRANSACT-FILE | Consider migrating to a modern database system for improved data management and access |
| CARDXREF | Indexed file for card and account cross-reference | Indexed File | XREF-FILE | Could be replaced with a relational database table for better data relationships |
| TRANTYPE | Indexed file for transaction type information | Indexed File | TRANTYPE-FILE | Potential for conversion to a lookup table in a database |
| TRANCATG | Indexed file for transaction category information | Indexed File | TRANCATG-FILE | Candidate for conversion to a database table for easier maintenance |
| TRANREPT | Output file for the generated report | Sequential File | REPORT-FILE | Consider modernizing to generate reports in formats like PDF or HTML |
| DATEPARM | Input file containing date range parameters | Sequential File | DATE-PARMS-FILE | Could be replaced with command-line arguments or configuration files |
| CVTRA05Y | Copy book for transaction record structure | Copy Book | COPY CVTRA05Y | Maintain for compatibility, but consider modernizing data structures |
| CVACT03Y | Copy book for card cross-reference record structure | Copy Book | COPY CVACT03Y | Evaluate for potential consolidation into a unified data model |
| CVTRA03Y | Copy book for transaction type record structure | Copy Book | COPY CVTRA03Y | Assess for integration into a more comprehensive data schema |
| CVTRA04Y | Copy book for transaction category record structure | Copy Book | COPY CVTRA04Y | Review for potential combination with other related structures |
| CVTRA07Y | Copy book for report record structure | Copy Book | COPY CVTRA07Y | Consider updating to support modern reporting formats |

These dependencies highlight the program's reliance on file-based data storage and processing, which are typical of legacy COBOL systems. In a modernization effort, these file-based dependencies could be replaced with more modern data storage and processing mechanisms, such as relational databases, APIs, or cloud-based storage solutions. This would improve data accessibility, reduce file I/O operations, and potentially enhance overall system performance and maintainability.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the CBTRN03C program. These structures are essential for processing transactions, generating reports, and managing the program's workflow.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | TRAN-RECORD | Group Item | 350 | Transaction record structure | Yes | N/A | Defined in CVTRA05Y copybook | Yes | Transaction |
| 1.1 | TRAN-ID | Alphanumeric | X(16) | Unique transaction identifier | Yes | N/A | Primary key for transaction | Yes | Transaction |
| 1.2 | TRAN-TYPE-CD | Alphanumeric | X(02) | Transaction type code | Yes | N/A | Used for transaction categorization | Yes | Transaction |
| 1.3 | TRAN-CAT-CD | Numeric | 9(04) | Transaction category code | Yes | N/A | Further categorizes the transaction | Yes | Transaction |
| 1.4 | TRAN-AMT | Numeric | S9(09)V99 | Transaction amount | Yes | N/A | Signed numeric with 2 decimal places | Yes | Financial |
| 1.5 | TRAN-CARD-NUM | Alphanumeric | X(16) | Card number used for transaction | Yes | N/A | Used for account lookup | Yes | Account |
| 1.6 | TRAN-PROC-TS | Alphanumeric | X(26) | Transaction processing timestamp | Yes | N/A | Used for date filtering | Yes | Temporal |
| 2 | CARD-XREF-RECORD | Group Item | 50 | Card cross-reference record | Yes | N/A | Defined in CVACT03Y copybook | Yes | Account |
| 2.1 | XREF-CARD-NUM | Alphanumeric | X(16) | Card number | Yes | N/A | Key for cross-reference lookup | Yes | Account |
| 2.2 | XREF-ACCT-ID | Numeric | 9(11) | Associated account ID | Yes | N/A | Used in report generation | Yes | Account |
| 3 | TRAN-TYPE-RECORD | Group Item | 60 | Transaction type record | Yes | N/A | Defined in CVTRA03Y copybook | Yes | Reference |
| 3.1 | TRAN-TYPE | Alphanumeric | X(02) | Transaction type code | Yes | N/A | Key for type lookup | Yes | Reference |
| 3.2 | TRAN-TYPE-DESC | Alphanumeric | X(50) | Transaction type description | Yes | N/A | Used in report generation | Yes | Reference |
| 4 | TRAN-CAT-RECORD | Group Item | 60 | Transaction category record | Yes | N/A | Defined in CVTRA04Y copybook | Yes | Reference |
| 4.1 | TRAN-CAT-KEY | Group Item | 6 | Composite key for category | Yes | N/A | Consists of type and category codes | Yes | Reference |
| 4.1.1 | TRAN-TYPE-CD | Alphanumeric | X(02) | Transaction type code | Yes | N/A | Part of composite key | Yes | Reference |
| 4.1.2 | TRAN-CAT-CD | Numeric | 9(04) | Transaction category code | Yes | N/A | Part of composite key | Yes | Reference |
| 4.2 | TRAN-CAT-TYPE-DESC | Alphanumeric | X(50) | Category description | Yes | N/A | Used in report generation | Yes | Reference |
| 5 | WS-DATEPARM-RECORD | Group Item | 21 | Date parameter record | Yes | N/A | Used for date range filtering | Yes | Control |
| 5.1 | WS-START-DATE | Alphanumeric | X(10) | Start date for report period | Yes | N/A | Format: YYYY-MM-DD | Yes | Temporal |
| 5.2 | WS-END-DATE | Alphanumeric | X(10) | End date for report period | Yes | N/A | Format: YYYY-MM-DD | Yes | Temporal |
| 6 | WS-REPORT-VARS | Group Item | N/A | Report control variables | Yes | N/A | Used for report generation logic | Yes | Control |
| 6.1 | WS-PAGE-TOTAL | Numeric | S9(09)V99 | Total amount for current page | Yes | 0 | Reset after each page | Yes | Financial |
| 6.2 | WS-ACCOUNT-TOTAL | Numeric | S9(09)V99 | Total amount for current account | Yes | 0 | Reset after each account | Yes | Financial |
| 6.3 | WS-GRAND-TOTAL | Numeric | S9(09)V99 | Total amount for entire report | Yes | 0 | Cumulative total | Yes | Financial |
| 6.4 | WS-CURR-CARD-NUM | Alphanumeric | X(16) | Current card number being processed | Yes | Spaces | Used for change detection | Yes | Control |

These data structures are crucial for the program's functionality and would be key considerations in any modernization effort. The transaction record (TRAN-RECORD) and various lookup records (CARD-XREF-RECORD, TRAN-TYPE-RECORD, TRAN-CAT-RECORD) could be transformed into database tables or objects in a modern system. The report variables (WS-REPORT-VARS) might be encapsulated in a report generation class or module in an object-oriented redesign.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Date Parameter Reading

- Detailed Description of the Rule:
  This rule reads the date parameters from the DATE-PARMS-FILE to determine the date range for transaction processing.
- What it proposes to do:
  Set the start and end dates for filtering transactions in the report.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open the DATE-PARMS-FILE for input.
2. Read a record from DATE-PARMS-FILE into WS-DATEPARM-RECORD.
3. Check the DATEPARM-STATUS:
   - If '00', continue processing.
   - If '10', set END-OF-FILE to 'Y' and terminate processing.
   - For any other status, display an error message, show the IO status, and abort the program.
4. Extract WS-START-DATE from positions 1-10 of WS-DATEPARM-RECORD.
5. Extract WS-END-DATE from positions 12-21 of WS-DATEPARM-RECORD.
6. Display the date range: "Reporting from [WS-START-DATE] to [WS-END-DATE]".
7. Close the DATE-PARMS-FILE.
8. Use WS-START-DATE and WS-END-DATE to filter transactions in the main processing loop:
   - Only process transactions where TRAN-PROC-TS (1:10) is greater than or equal to WS-START-DATE and less than or equal to WS-END-DATE.
9. Use WS-START-DATE and WS-END-DATE in the report header (REPT-START-DATE and REPT-END-DATE fields).
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Transaction Processing

- Detailed Description of the Rule:
  This rule processes each transaction record, filtering by date and generating report details.
- What it proposes to do:
  Read transactions, apply date filter, lookup additional information, and write report entries.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open all required files: transaction file, report file, card cross-reference file, transaction type file, transaction category file, and date parameter file.
2. Read the date parameter file to get WS-START-DATE and WS-END-DATE.
3. Read a transaction record from TRANSACT-FILE into TRAN-RECORD.
4. If end-of-file is reached, perform totals writing and exit the loop.
5. Check if TRAN-PROC-TS (positions 1-10) is within WS-START-DATE and WS-END-DATE range.
   - If not within range, skip to next record.
6. If WS-CURR-CARD-NUM is different from TRAN-CARD-NUM:
   - If not first time, perform 1120-WRITE-ACCOUNT-TOTALS.
   - Update WS-CURR-CARD-NUM with TRAN-CARD-NUM.
   - Perform card cross-reference lookup using TRAN-CARD-NUM.
7. Perform transaction type lookup using TRAN-TYPE-CD.
8. Perform transaction category lookup using TRAN-TYPE-CD and TRAN-CAT-CD.
9. If it's the first transaction (WS-FIRST-TIME = 'Y'):
   - Set WS-FIRST-TIME to 'N'.
   - Write report headers.
10. If WS-LINE-COUNTER modulo WS-PAGE-SIZE equals 0:
    - Perform 1110-WRITE-PAGE-TOTALS.
    - Write headers for new page.
11. Add TRAN-AMT to WS-PAGE-TOTAL and WS-ACCOUNT-TOTAL.
12. Write transaction detail to report.
13. Add 1 to WS-LINE-COUNTER.
14. Repeat from step 3 until end-of-file is reached.
15. After processing all records:
    - Write final page totals.
    - Write grand totals.
16. Close all opened files.

## Additional Details:
- The program uses multiple indexed files for lookups: XREF-FILE, TRANTYPE-FILE, and TRANCATG-FILE.
- Error handling is implemented for file operations, with specific error messages and abend procedures.
- The report includes page totals, account totals, and grand totals.
- The page size is set to 20 lines (WS-PAGE-SIZE).
- The program uses various working storage variables to track totals and control flow.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Card Cross-Reference Lookup

- Detailed Description of the Rule:
  This rule looks up the account information for a given card number.
- What it proposes to do:
  Retrieve the account ID associated with a card number.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-XREF-CARD-NUM to the current TRAN-CARD-NUM.
2. Read XREF-FILE record using FD-XREF-CARD-NUM as the key.
3. If the read is successful:
   - Extract XREF-ACCT-ID from the CARD-XREF-RECORD.
4. If the read fails (invalid key):
   - Display error message: "INVALID CARD NUMBER: [FD-XREF-CARD-NUM]"
   - Set IO-STATUS to 23.
   - Display IO status details by performing 9910-DISPLAY-IO-STATUS.
   - Abort the program by performing 9999-ABEND-PROGRAM, which sets TIMING to 0, ABCODE to 999, and calls 'CEE3ABD'.
5. If successful, use the retrieved XREF-ACCT-ID for further processing in the transaction report generation.

Note: This lookup is part of a larger transaction reporting process that includes date range filtering, transaction type and category lookups, and detailed report generation with running totals.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Transaction Type Lookup

- Detailed Description of the Rule:
  This rule retrieves the description for a given transaction type code and performs additional lookups for transaction category and card cross-reference information.
- What it proposes to do:
  Get the transaction type description, transaction category description, and account information for reporting.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-TRAN-TYPE to TRAN-TYPE-CD from the current transaction record.
2. Read TRANTYPE-FILE record using FD-TRAN-TYPE as the key.
3. If the read is successful:
   - Extract TRAN-TYPE-DESC from the record.
4. If the read fails (invalid key):
   - Display error message: "INVALID TRANSACTION TYPE: [FD-TRAN-TYPE]"
   - Set IO-STATUS to 23.
   - Display IO status details.
   - Abort the program.
5. Set FD-TRAN-TYPE-CD of FD-TRAN-CAT-KEY to TRAN-TYPE-CD from the current transaction record.
6. Set FD-TRAN-CAT-CD of FD-TRAN-CAT-KEY to TRAN-CAT-CD from the current transaction record.
7. Read TRANCATG-FILE record using FD-TRAN-CAT-KEY as the key.
8. If the read is successful:
   - Extract TRAN-CAT-TYPE-DESC from the record.
9. If the read fails (invalid key):
   - Display error message: "INVALID TRAN CATG KEY: [FD-TRAN-CAT-KEY]"
   - Set IO-STATUS to 23.
   - Display IO status details.
   - Abort the program.
10. If the current card number (TRAN-CARD-NUM) is different from the previous one (WS-CURR-CARD-NUM):
    - Set FD-XREF-CARD-NUM to TRAN-CARD-NUM.
    - Read XREF-FILE record using FD-XREF-CARD-NUM as the key.
    - If the read is successful:
      - Extract XREF-ACCT-ID from the record.
    - If the read fails (invalid key):
      - Display error message: "INVALID CARD NUMBER: [FD-XREF-CARD-NUM]"
      - Set IO-STATUS to 23.
      - Display IO status details.
      - Abort the program.
11. Use the retrieved information (TRAN-TYPE-DESC, TRAN-CAT-TYPE-DESC, XREF-ACCT-ID) along with other transaction details for reporting purposes.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Transaction Category Lookup

- Detailed Description of the Rule:
  This rule retrieves the description for a given transaction category code.
- What it proposes to do:
  Get the transaction category description for reporting.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-TRAN-TYPE-CD of FD-TRAN-CAT-KEY to TRAN-TYPE-CD from the current transaction record.
2. Set FD-TRAN-CAT-CD of FD-TRAN-CAT-KEY to TRAN-CAT-CD from the current transaction record.
3. Read TRANCATG-FILE record using FD-TRAN-CAT-KEY as the key.
4. If the read is successful:
   - Extract TRAN-CAT-TYPE-DESC from the record.
5. If the read fails (invalid key):
   - Display error message: "INVALID TRAN CATG KEY: [FD-TRAN-CAT-KEY]"
   - Set IO-STATUS to 23.
   - Display IO status details by performing the 9910-DISPLAY-IO-STATUS routine.
   - Abort the program by performing the 9999-ABEND-PROGRAM routine, which sets TIMING to 0, ABCODE to 999, and calls 'CEE3ABD'.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Write Transaction Detail

- Detailed Description of the Rule:
  This rule writes a single transaction detail line to the report, including page, account, and grand totals.
- What it proposes to do:
  Format and output transaction information in the report, manage page breaks, and calculate totals.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. If this is the first transaction (WS-FIRST-TIME = 'Y'):
   - Set WS-FIRST-TIME to 'N'
   - Write report headers including start and end dates from WS-DATEPARM-RECORD
2. If the line counter (WS-LINE-COUNTER) modulo page size (WS-PAGE-SIZE) is 0:
   - Write page totals
   - Write new page headers
3. Add TRAN-AMT to WS-PAGE-TOTAL and WS-ACCOUNT-TOTAL
4. Initialize TRANSACTION-DETAIL-REPORT structure
5. Set TRAN-REPORT-TRANS-ID to TRAN-ID
6. Set TRAN-REPORT-ACCOUNT-ID to XREF-ACCT-ID
7. Set TRAN-REPORT-TYPE-CD to TRAN-TYPE-CD of TRAN-RECORD
8. Set TRAN-REPORT-TYPE-DESC to TRAN-TYPE-DESC
9. Set TRAN-REPORT-CAT-CD to TRAN-CAT-CD of TRAN-RECORD
10. Set TRAN-REPORT-CAT-DESC to TRAN-CAT-TYPE-DESC
11. Set TRAN-REPORT-SOURCE to TRAN-SOURCE
12. Set TRAN-REPORT-AMT to TRAN-AMT
13. Move TRANSACTION-DETAIL-REPORT to FD-REPTFILE-REC
14. Write FD-REPTFILE-REC to REPORT-FILE
15. If write is successful:
    - Increment WS-LINE-COUNTER by 1
16. If write fails:
    - Display error message: "ERROR WRITING REPTFILE"
    - Display IO status details
    - Abort the program
17. If the current card number (WS-CURR-CARD-NUM) is different from TRAN-CARD-NUM:
    - If not the first transaction, write account totals
    - Update WS-CURR-CARD-NUM with TRAN-CARD-NUM
    - Look up new card details in XREF-FILE
18. Look up transaction type details in TRANTYPE-FILE
19. Look up transaction category details in TRANCATG-FILE
20. At end of file:
    - Write final page totals
    - Write account totals
    - Write grand totals (WS-GRAND-TOTAL)

Note: The program processes transactions within the date range specified by WS-START-DATE and WS-END-DATE from the DATEPARM file.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Write Page Totals

- Detailed Description of the Rule:
  This rule writes the page totals, resets the page total counter, and handles various aspects of report formatting and totaling.
- What it proposes to do:
  Sum up and display the total amount for transactions on the current page, manage page breaks, and handle account and grand totals.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Move WS-PAGE-TOTAL to REPT-PAGE-TOTAL.
2. Move REPORT-PAGE-TOTALS to FD-REPTFILE-REC.
3. Write FD-REPTFILE-REC to REPORT-FILE.
4. Add WS-PAGE-TOTAL to WS-GRAND-TOTAL.
5. Reset WS-PAGE-TOTAL to 0.
6. Add 1 to WS-LINE-COUNTER.
7. Move TRANSACTION-HEADER-2 to FD-REPTFILE-REC.
8. Write FD-REPTFILE-REC to REPORT-FILE.
9. Add 1 to WS-LINE-COUNTER.
10. After writing page totals, check if it's the end of an account:
    - If it is, perform the following steps:
      a. Move WS-ACCOUNT-TOTAL to REPT-ACCOUNT-TOTAL.
      b. Move REPORT-ACCOUNT-TOTALS to FD-REPTFILE-REC.
      c. Write FD-REPTFILE-REC to REPORT-FILE.
      d. Reset WS-ACCOUNT-TOTAL to 0.
      e. Add 1 to WS-LINE-COUNTER.
      f. Move TRANSACTION-HEADER-2 to FD-REPTFILE-REC.
      g. Write FD-REPTFILE-REC to REPORT-FILE.
      h. Add 1 to WS-LINE-COUNTER.
11. At the end of all processing, write grand totals:
    - Move WS-GRAND-TOTAL to REPT-GRAND-TOTAL.
    - Move REPORT-GRAND-TOTALS to FD-REPTFILE-REC.
    - Write FD-REPTFILE-REC to REPORT-FILE.
12. For each write operation to REPORT-FILE:
    - Check if TRANREPT-STATUS is '00'.
    - If not '00', display an error message, show the IO status, and abend the program.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Write Account Totals

- Detailed Description of the Rule:
  This rule writes the account totals when switching to a new card number and performs other related operations.
- What it proposes to do:
  Sum up and display the total amount for all transactions for a specific account, write page totals, and reset counters.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Move WS-ACCOUNT-TOTAL to REPT-ACCOUNT-TOTAL.
2. Move REPORT-ACCOUNT-TOTALS to FD-REPTFILE-REC.
3. Write FD-REPTFILE-REC to REPORT-FILE.
4. Check if the write operation was successful:
   - If TRANREPT-STATUS = '00', continue.
   - Otherwise, display 'ERROR WRITING REPTFILE', show IO status, and abend the program.
5. Move 0 to WS-ACCOUNT-TOTAL.
6. Add 1 to WS-LINE-COUNTER.
7. Move TRANSACTION-HEADER-2 to FD-REPTFILE-REC.
8. Write FD-REPTFILE-REC to REPORT-FILE.
9. Check if the write operation was successful:
   - If TRANREPT-STATUS = '00', continue.
   - Otherwise, display 'ERROR WRITING REPTFILE', show IO status, and abend the program.
10. Add 1 to WS-LINE-COUNTER.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 9
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Write Grand Totals

- Detailed Description of the Rule:
  This rule writes the grand total for all transactions in the report.
- What it proposes to do:
  Display the sum of all transaction amounts processed in the report.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Move WS-GRAND-TOTAL to REPT-GRAND-TOTAL.
2. Move REPORT-GRAND-TOTALS to FD-REPTFILE-REC.
3. Write FD-REPTFILE-REC to REPORT-FILE.
4. If write is successful (TRANREPT-STATUS = '00'):
   - Set APPL-RESULT to 0.
5. If write fails (TRANREPT-STATUS not = '00'):
   - Set APPL-RESULT to 12.
6. If APPL-RESULT is 0 (APPL-AOK), continue processing.
7. If APPL-RESULT is not 0:
   - Display error message: "ERROR WRITING REPTFILE"
   - Move TRANREPT-STATUS to IO-STATUS.
   - Perform 9910-DISPLAY-IO-STATUS to show detailed file status.
   - Perform 9999-ABEND-PROGRAM to abort the program:
     - Display "ABENDING PROGRAM"
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to terminate the program
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 10
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Write Report Headers

- Detailed Description of the Rule:
  This rule writes the report headers at the beginning of the report and each new page, including the date range for the report.
- What it proposes to do:
  Output formatted headers containing report title, date range, and column headers.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. If this is the first time writing headers (WS-FIRST-TIME = 'Y'):
   - Set WS-FIRST-TIME to 'N'
   - Move WS-START-DATE to REPT-START-DATE
   - Move WS-END-DATE to REPT-END-DATE
2. Move REPORT-NAME-HEADER to FD-REPTFILE-REC.
3. Write FD-REPTFILE-REC to REPORT-FILE.
4. Increment WS-LINE-COUNTER by 1.
5. Move WS-BLANK-LINE to FD-REPTFILE-REC.
6. Write FD-REPTFILE-REC to REPORT-FILE.
7. Increment WS-LINE-COUNTER by 1.
8. Move TRANSACTION-HEADER-1 to FD-REPTFILE-REC.
9. Write FD-REPTFILE-REC to REPORT-FILE.
10. Increment WS-LINE-COUNTER by 1.
11. Move TRANSACTION-HEADER-2 to FD-REPTFILE-REC.
12. Write FD-REPTFILE-REC to REPORT-FILE.
13. Increment WS-LINE-COUNTER by 1.
14. For each write operation:
    - If successful (TRANREPT-STATUS = '00'), continue.
    - If failed, display error message "ERROR WRITING REPTFILE", show IO status, and abort the program.
15. This process is repeated when WS-LINE-COUNTER modulo WS-PAGE-SIZE equals 0, indicating a new page is needed.
16. Before writing headers on a new page (except the first time), perform the following:
    - Write page totals
    - Reset WS-PAGE-TOTAL to 0
    - Write TRANSACTION-HEADER-2 again
```

<!--rule-end-->