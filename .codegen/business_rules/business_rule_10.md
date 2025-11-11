# CSUTLDTC.cbl: Date Validation Utility

## Overview
This COBOL program, CSUTLDTC, is a utility module designed to validate dates using the CEEDAYS API. It serves as a crucial component in the overall project by providing a standardized method for date validation, which is essential for maintaining data integrity across various business processes.

The main purpose of this file is to:
1. Accept a date and its format as input
2. Validate the date using the CEEDAYS API
3. Return the validation result and any error messages

This utility contributes to the overall functionality of the system by ensuring that dates used in various transactions and processes are valid and conform to the expected format, thus preventing data inconsistencies and potential errors in downstream processes.

<!-- general-rule-start -->
## General Business Rules:

1. Date Input and Format:
   - The program accepts two main inputs: a date (LS-DATE) and its format (LS-DATE-FORMAT), both as 10-character strings.
   - These inputs are passed to the CEEDAYS API for validation.

2. Date Validation Process:
   - The program uses the CEEDAYS API to validate the input date against the specified format.
   - CEEDAYS attempts to convert the input date to a Lilian date (days since 14 October 1582).

3. Validation Results:
   - The program interprets the feedback from CEEDAYS and categorizes the result into various scenarios:
     - Valid date
     - Insufficient data
     - Bad date value
     - Invalid era
     - Unsupported range
     - Invalid month
     - Bad picture string
     - Non-numeric data
     - Year in era is zero

4. Error Handling:
   - For each possible error scenario, the program sets a specific message in the WS-RESULT field.
   - The severity of the error is captured in the WS-SEVERITY field.

5. Output:
   - The program constructs a detailed message (WS-MESSAGE) containing:
     - Error severity
     - Message code
     - Result description
     - Test date
     - Date format used

6. Return Values:
   - The constructed message is returned in the LS-RESULT field.
   - The severity of the result is set as the program's return code.

7. Integration:
   - This utility is designed to be called by other programs in the system that require date validation.
   - It provides a standardized way of handling date validations across the entire application.

8. Error Codes:
   - The program uses predefined condition names (e.g., FC-INVALID-DATE, FC-INSUFFICIENT-DATA) to interpret the feedback from CEEDAYS.
   - These condition names correspond to specific error scenarios and help in precise error reporting.

9. Date Format Flexibility:
   - The program allows for different date formats to be specified, making it versatile for various date representations used in the system.

This utility plays a critical role in ensuring data quality and consistency across the system by providing a centralized and standardized method for date validation. It helps prevent issues related to invalid dates that could potentially cause errors in financial calculations, reporting, or other date-dependent processes in the credit card management system.
<!-- general-rule-end -->
## Dependencies

This program relies on external APIs and standard COBOL structures for its functionality. The main dependency is the CEEDAYS API, which is crucial for date validation. Understanding these dependencies is important for maintaining and potentially modernizing the application.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CEEDAYS | API for date validation and conversion | External API | IBM Language Environment | Critical for date processing; may need to be replaced or wrapped in a modernized system |
| COBOL Standard Libraries | Standard COBOL libraries for basic operations | Language Library | COBOL Compiler | Essential for COBOL operations; may need to be replaced or adapted in a new language |
| Calling Program | The program that calls CSUTLDTC and provides input parameters | External Program | Not specified | Interface may need to be adapted or modernized for integration with new systems |
| LS-DATE | Input parameter for the date to be validated | Input Structure | Defined in Linkage Section | Data format may need to be standardized or converted in modernization |
| LS-DATE-FORMAT | Input parameter specifying the format of the input date | Input Structure | Defined in Linkage Section | Format specifications may need to be updated or standardized |
| LS-RESULT | Output parameter for returning the validation result | Output Structure | Defined in Linkage Section | Output format may need to be adapted for modern interfaces or logging systems |
## Detailed Rules
## Data Structure

This section describes the detailed data structure of the application, focusing on the key fields used in the CSUTLDTC program for date validation.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | WS-DATE-TO-TEST | Group Item | Variable | Structure for input date | Yes | None | Contains Vstring-length and Vstring-text | Yes | Input |
| 1.1 | Vstring-length | Binary | 2 bytes | Length of input date string | Yes | None | Part of WS-DATE-TO-TEST | Yes | Input |
| 1.2 | Vstring-text | Group Item | Variable | Text of input date | Yes | None | Contains Vstring-char | Yes | Input |
| 1.2.1 | Vstring-char | Alphanumeric | 1 byte (occurs 0 to 256 times) | Individual characters of input date | Yes | None | Occurs depending on Vstring-length | Yes | Input |
| 2 | WS-DATE-FORMAT | Group Item | Variable | Structure for date format | Yes | None | Similar structure to WS-DATE-TO-TEST | Yes | Input |
| 3 | OUTPUT-LILLIAN | Binary | 4 bytes | Lilian date output from CEEDAYS | No | 0 | Represents days since 14 Oct 1582 | Yes | Output |
| 4 | WS-MESSAGE | Group Item | 80 bytes | Structure for output message | Yes | Spaces | Contains multiple subfields | Yes | Output |
| 4.1 | WS-SEVERITY | Alphanumeric | 4 bytes | Severity of the result | Yes | Spaces | Can be interpreted as numeric | Yes | Output |
| 4.2 | WS-MSG-NO | Alphanumeric | 4 bytes | Message number | Yes | Spaces | Can be interpreted as numeric | Yes | Output |
| 4.3 | WS-RESULT | Alphanumeric | 15 bytes | Result of date validation | Yes | Spaces | Contains descriptive text | Yes | Output |
| 4.4 | WS-DATE | Alphanumeric | 10 bytes | Input date (for display) | Yes | Spaces | Copy of input date | Yes | Output |
| 4.5 | WS-DATE-FMT | Alphanumeric | 10 bytes | Input date format (for display) | Yes | Spaces | Copy of input date format | Yes | Output |
| 5 | FEEDBACK-CODE | Group Item | 16 bytes | Feedback from CEEDAYS API | Yes | None | Contains multiple subfields | Yes | Input/Output |
| 6 | LS-DATE | Alphanumeric | 10 bytes | Input date from calling program | Yes | None | Linkage section item | Yes | Input |
| 7 | LS-DATE-FORMAT | Alphanumeric | 10 bytes | Input date format from calling program | Yes | None | Linkage section item | Yes | Input |
| 8 | LS-RESULT | Alphanumeric | 80 bytes | Output result to calling program | Yes | None | Linkage section item | Yes | Output |



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Date Validation

- Detailed Description of the Rule:
  This rule validates a given date string against a specified format using the CEEDAYS API. It determines if the date is valid and provides detailed feedback on any errors encountered during the validation process.

- What it proposes to do:
  The rule aims to ensure that dates used in the system are valid and conform to the expected format, preventing data inconsistencies and potential errors in downstream processes.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize the working storage variables:
   - Set WS-MESSAGE to spaces
   - Set WS-DATE to spaces

2. Prepare the input date for CEEDAYS:
   - Set VSTRING-LENGTH of WS-DATE-TO-TEST to the length of LS-DATE
   - Move LS-DATE to VSTRING-TEXT of WS-DATE-TO-TEST and to WS-DATE

3. Prepare the date format for CEEDAYS:
   - Set VSTRING-LENGTH of WS-DATE-FORMAT to the length of LS-DATE-FORMAT
   - Move LS-DATE-FORMAT to VSTRING-TEXT of WS-DATE-FORMAT and to WS-DATE-FMT

4. Initialize OUTPUT-LILLIAN to 0

5. Call the CEEDAYS API with the following parameters:
   - WS-DATE-TO-TEST (input date)
   - WS-DATE-FORMAT (date format)
   - OUTPUT-LILLIAN (output Lilian date)
   - FEEDBACK-CODE (feedback from the API)

6. Process the FEEDBACK-CODE:
   - Move SEVERITY of FEEDBACK-CODE to WS-SEVERITY-N
   - Move MSG-NO of FEEDBACK-CODE to WS-MSG-NO-N

7. Evaluate the FEEDBACK-CODE and set WS-RESULT based on the following conditions:
   - If FC-INVALID-DATE, set WS-RESULT to 'Date is valid'
   - If FC-INSUFFICIENT-DATA, set WS-RESULT to 'Insufficient'
   - If FC-BAD-DATE-VALUE, set WS-RESULT to 'Datevalue error'
   - If FC-INVALID-ERA, set WS-RESULT to 'Invalid Era    '
   - If FC-UNSUPP-RANGE, set WS-RESULT to 'Unsupp. Range  '
   - If FC-INVALID-MONTH, set WS-RESULT to 'Invalid month  '
   - If FC-BAD-PIC-STRING, set WS-RESULT to 'Bad Pic String '
   - If FC-NON-NUMERIC-DATA, set WS-RESULT to 'Nonnumeric data'
   - If FC-YEAR-IN-ERA-ZERO, set WS-RESULT to 'YearInEra is 0 '
   - For any other condition, set WS-RESULT to 'Date is invalid'

8. Construct the output message (WS-MESSAGE):
   - WS-SEVERITY (4 characters)
   - 'Mesg Code:' (literal)
   - WS-MSG-NO (4 characters)
   - WS-RESULT (15 characters)
   - 'TstDate:' (literal)
   - WS-DATE (10 characters)
   - 'Mask used:' (literal)
   - WS-DATE-FMT (10 characters)

9. Return the results:
   - Move WS-MESSAGE to LS-RESULT
   - Move WS-SEVERITY-N to RETURN-CODE

Validation Rules:
- The input date (LS-DATE) must be a 10-character string
- The input date format (LS-DATE-FORMAT) must be a 10-character string
- The CEEDAYS API determines the validity of the date based on the provided format
- The severity of the result (WS-SEVERITY-N) is used as the return code for the program
- The OUTPUT-LILLIAN field is not used in the final output or decision-making process
```

<!--rule-end-->