# COACTUPC.cbl: Account Update Processing

## Overview
This COBOL program, COACTUPC, is responsible for handling account updates in a credit card management system. It provides functionality to view and modify account and customer details. The program interacts with multiple files, including account, customer, and card cross-reference files, to retrieve and update information.

<!-- general-rule-start -->
## General Business Rules:

1. The program allows users to view and update account and customer information.
2. It performs various data validations to ensure the integrity of the information being updated.
3. The program handles both account and customer data updates in a single transaction.
4. It checks for any changes made to the records by other users before committing updates.
5. The program supports different user types (admin and regular users) and handles screen navigation accordingly.
6. It provides error handling and informative messages to guide users through the update process.
7. The program uses CICS (Customer Information Control System) for transaction management and file operations.

Key functionalities include:
- Retrieving account and customer data based on the account ID
- Displaying account and customer information on the screen
- Allowing users to modify various fields such as account status, credit limits, personal information, etc.
- Validating user inputs for data integrity
- Updating account and customer records in their respective files
- Handling various error scenarios and providing appropriate feedback to the user

The program follows a structured approach with distinct sections for data retrieval, input processing, screen display, and data update operations. It uses copybooks for common data structures and routines, promoting code reusability and maintainability.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies, including CICS commands for transaction management, file operations, and screen handling. It also uses various copybooks for shared data structures and common routines. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System for transaction management and file operations | System | N/A | High - May need to be replaced or adapted in a modernized system |
| COACTUP | Copybook for Account Update Screen Layout | Copybook | COACTUP.CPY | Medium - Screen layout may need to be redesigned for modern interfaces |
| COCOM01Y | Copybook for CardDemo application programs communication area | Copybook | COCOM01Y.cpy | High - Contains shared data structures that may need to be refactored |
| CSDAT01Y | Copybook for Date related code | Copybook | CSDAT01Y.cpy | Medium - Date handling may need to be updated for modern systems |
| CSLKPCDY | Copybook for Lookup code repository | Copybook | CSLKPCDY.cpy | Medium - Lookup codes may need to be stored differently in a modernized system |
| CSMSG01Y | Copybook for Common Messages | Copybook | CSMSG01Y.cpy | Low - Messages may need to be internationalized or centralized |
| CSMSG02Y | Copybook for Abend Variables | Copybook | CSMSG02Y.cpy | Medium - Error handling may need to be redesigned |
| CSUSR01Y | Copybook for Signed on user data | Copybook | CSUSR01Y.cpy | Medium - User authentication may change in a modernized system |
| CVACT01Y | Copybook for ACCT RECORD LAYOUT | Copybook | CVACT01Y.cpy | High - Core data structure that may need to be adapted |
| CVACT03Y | Copybook for CARD XREF LAYOUT | Copybook | CVACT03Y.cpy | High - Core data structure that may need to be adapted |
| CVCUS01Y | Copybook for CUSTOMER LAYOUT | Copybook | CVCUS01Y.cpy | High - Core data structure that may need to be adapted |
| CSUTLDPY | Copybook for Common Date Routines | Copybook | CSUTLDPY.cpy | Medium - Date handling may need to be updated for modern systems |
| CSSTRPFY | Copybook for Common code to store PFKey | Copybook | CSSTRPFY.cpy | Low - Function key handling may be replaced in modern interfaces |
| ACCTDAT | Account data file | File | N/A | High - Core data storage that may need to be migrated to a modern database |
| CUSTDAT | Customer data file | File | N/A | High - Core data storage that may need to be migrated to a modern database |
| CARDDAT | Card data file | File | N/A | High - Core data storage that may need to be migrated to a modern database |
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COACTUPC program, focusing on the account and customer information that can be updated.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | ACUP-NEW-ACCT-ID | Numeric | 11 | Account ID | Yes | None | Primary key for account | High | Account |
| 2 | ACUP-NEW-ACTIVE-STATUS | Character | 1 | Account active status | Yes | None | 'Y' for active, 'N' for inactive | High | Account |
| 3 | ACUP-NEW-CURR-BAL-N | Numeric | 12,2 | Current balance | Yes | 0 | Signed amount | High | Account |
| 4 | ACUP-NEW-CREDIT-LIMIT-N | Numeric | 12,2 | Credit limit | Yes | 0 | Signed amount | High | Account |
| 5 | ACUP-NEW-CASH-CREDIT-LIMIT-N | Numeric | 12,2 | Cash credit limit | Yes | 0 | Signed amount | High | Account |
| 6 | ACUP-NEW-OPEN-DATE | Date | 8 | Account open date | Yes | None | Format: YYYYMMDD | Medium | Account |
| 7 | ACUP-NEW-EXPIRAION-DATE | Date | 8 | Account expiration date | Yes | None | Format: YYYYMMDD | Medium | Account |
| 8 | ACUP-NEW-REISSUE-DATE | Date | 8 | Card reissue date | Yes | None | Format: YYYYMMDD | Medium | Account |
| 9 | ACUP-NEW-CURR-CYC-CREDIT-N | Numeric | 12,2 | Current cycle credit | Yes | 0 | Signed amount | High | Account |
| 10 | ACUP-NEW-CURR-CYC-DEBIT-N | Numeric | 12,2 | Current cycle debit | Yes | 0 | Signed amount | High | Account |
| 11 | ACUP-NEW-GROUP-ID | Character | 10 | Account group ID | No | Spaces | Grouping identifier | Medium | Account |
| 12 | ACUP-NEW-CUST-ID | Numeric | 9 | Customer ID | Yes | None | Primary key for customer | High | Customer |
| 13 | ACUP-NEW-CUST-SSN | Numeric | 9 | Social Security Number | Yes | None | Format: XXXXXXXXX | High | Customer |
| 14 | ACUP-NEW-CUST-FIRST-NAME | Character | 25 | Customer first name | Yes | None | Alphabetic characters only | High | Customer |
| 15 | ACUP-NEW-CUST-MIDDLE-NAME | Character | 25 | Customer middle name | No | Spaces | Alphabetic characters only | Medium | Customer |
| 16 | ACUP-NEW-CUST-LAST-NAME | Character | 25 | Customer last name | Yes | None | Alphabetic characters only | High | Customer |
| 17 | ACUP-NEW-CUST-ADDR-LINE-1 | Character | 50 | Address line 1 | Yes | None | Street address | High | Customer |
| 18 | ACUP-NEW-CUST-ADDR-LINE-2 | Character | 50 | Address line 2 | No | Spaces | Additional address info | Medium | Customer |
| 19 | ACUP-NEW-CUST-ADDR-LINE-3 | Character | 50 | City | Yes | None | City name | High | Customer |
| 20 | ACUP-NEW-CUST-ADDR-STATE-CD | Character | 2 | State code | Yes | None | US state code | High | Customer |
| 21 | ACUP-NEW-CUST-ADDR-COUNTRY-CD | Character | 3 | Country code | Yes | None | ISO country code | High | Customer |
| 22 | ACUP-NEW-CUST-ADDR-ZIP | Character | 10 | ZIP code | Yes | None | US ZIP code | High | Customer |
| 23 | ACUP-NEW-CUST-PHONE-NUM-1 | Character | 15 | Primary phone number | Yes | None | Format: (XXX)XXX-XXXX | High | Customer |
| 24 | ACUP-NEW-CUST-PHONE-NUM-2 | Character | 15 | Secondary phone number | No | Spaces | Format: (XXX)XXX-XXXX | Medium | Customer |
| 25 | ACUP-NEW-CUST-DOB-YYYY-MM-DD | Date | 8 | Date of birth | Yes | None | Format: YYYYMMDD | High | Customer |
| 26 | ACUP-NEW-CUST-GOVT-ISSUED-ID | Character | 20 | Government issued ID | No | Spaces | ID number | Medium | Customer |
| 27 | ACUP-NEW-CUST-EFT-ACCOUNT-ID | Character | 10 | EFT account ID | No | Spaces | Electronic Funds Transfer ID | Medium | Customer |
| 28 | ACUP-NEW-CUST-PRI-HOLDER-IND | Character | 1 | Primary card holder indicator | Yes | 'N' | 'Y' for primary, 'N' for secondary | High | Customer |
| 29 | ACUP-NEW-CUST-FICO-SCORE | Numeric | 3 | FICO credit score | Yes | None | Range: 300-850 | High | Customer |



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Receive and Process User Inputs

- Detailed Description of the Rule:
  This rule handles the reception of user inputs from the screen and processes them for further use in the program. It captures all the fields displayed on the account update screen and stores them in the program's working storage.

- What it proposes to do:
  The purpose is to gather all user-entered data, perform initial data transformations, and prepare the data for validation and processing.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Receive the map data from the screen using CICS RECEIVE MAP command.
2. For each field on the screen:
   a. Check if the field is empty, contains '*', or spaces.
   b. If empty, '*', or spaces, set the corresponding program variable to LOW-VALUES.
   c. If not empty, move the screen value to the corresponding program variable.
3. For numeric fields (e.g., credit limits, balances, FICO score):
   a. If the field is not empty, attempt to convert it to a numeric value using FUNCTION NUMVAL-C.
   b. If conversion is successful, store the numeric value.
   c. If conversion fails, leave the field as is for later validation.
4. For date fields (e.g., open date, expiry date, reissue date, date of birth):
   a. Split the date into separate year, month, and day components.
   b. Store each component in separate program variables.
5. For phone numbers:
   a. Split the phone number into area code, prefix, and line number.
   b. Store each component in separate program variables.
6. For SSN:
   a. Split the SSN into three parts (XXX-XX-XXXX format).
   b. Store each part in separate program variables.
7. For all other fields, store the data as is for later processing and validation.
8. Initialize all edit flags and error messages.
9. Set the appropriate flags based on the current state of the transaction (e.g., ACUP-DETAILS-NOT-FETCHED, ACUP-SHOW-DETAILS, etc.).
10. If this is a fresh entry into the program, initialize the working storage and set appropriate flags.
11. If changes have been made and confirmed, reset the search keys and prepare for a new search.
12. Handle specific scenarios:
    a. If exiting the program (PF03 pressed), prepare for transfer to the calling program or main menu.
    b. If showing details for the first time, set up the screen for data display.
    c. If changes have been made, prepare for validation and confirmation.
13. Store all received data in the program's working storage for further processing and validation.
```

This revised rule now accurately reflects the detailed logic and data handling present in the COBOL program, including the specific checks for empty fields, the conversion of numeric fields, the splitting of composite fields like dates and phone numbers, and the handling of various transaction states.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Validate User Inputs

- Detailed Description of the Rule:
  This rule performs comprehensive validation on all user inputs to ensure data integrity and adherence to business rules.

- What it proposes to do:
  The purpose is to check all entered data for correctness, format compliance, and business rule adherence before allowing any updates to the database.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. For each input field:
   a. Check if the field is required and not blank.
   b. If required and blank, set an error flag and prepare an error message.
2. For the Account Status field:
   a. Check if the value is 'Y' or 'N'.
   b. If not, set an error flag and prepare an error message.
3. For numeric fields (credit limits, balances):
   a. Check if the value is numeric.
   b. Check if it's within acceptable range (e.g., not negative for balances).
   c. If either check fails, set an error flag and prepare an error message.
4. For date fields (Open Date, Expiry Date, Reissue Date):
   a. Validate each component (year, month, day) individually:
      - Year should be a valid 4-digit year.
      - Month should be between 01 and 12.
      - Day should be valid for the given month (considering leap years for February).
   b. Check if the date is logical (e.g., not in the future for open date).
   c. If any check fails, set an error flag and prepare an error message.
5. For the SSN:
   a. Check if it's a valid 9-digit number.
   b. Verify that it doesn't start with 000, 666, or 900-999.
   c. Verify that the middle two digits are not 00.
   d. Verify that the last four digits are not 0000.
   e. If invalid, set an error flag and prepare an error message.
6. For phone numbers:
   a. Validate each component (area code, prefix, line number):
      - Area code should be a valid 3-digit code (not 000 or 911).
      - Prefix should be a valid 3-digit number (not 000).
      - Line number should be a valid 4-digit number (not 0000).
   b. If invalid, set an error flag and prepare an error message.
7. For name fields:
   a. Check if they contain only alphabetic characters and spaces.
   b. If not, set an error flag and prepare an error message.
8. For address fields:
   a. Validate state code against a list of valid US state codes.
   b. Validate zip code format (5 digits).
   c. Validate that the zip code is valid for the given state.
   d. If invalid, set an error flag and prepare an error message.
9. For the FICO score:
   a. Check if it's a valid number between 300 and 850.
   b. If not, set an error flag and prepare an error message.
10. For the Date of Birth:
    a. Validate the date format (YYYY-MM-DD).
    b. Check if the date is in the past.
    c. Calculate the age and ensure it's at least 18 years old.
    d. If any check fails, set an error flag and prepare an error message.
11. For the EFT Account ID:
    a. Check if it's a valid 10-digit number.
    b. If not, set an error flag and prepare an error message.
12. For the Primary Card Holder indicator:
    a. Check if the value is 'Y' or 'N'.
    b. If not, set an error flag and prepare an error message.
13. After all validations:
    a. If any error flags are set, compile all error messages.
    b. If no errors, set a flag indicating all inputs are valid.
14. If any data has changed:
    a. Compare new values with old values stored in ACUP-OLD-DETAILS.
    b. If changes are detected, set CHANGE-HAS-OCCURRED to TRUE.
    c. If no changes are detected, set NO-CHANGES-FOUND to TRUE.
15. If changes are made and validated:
    a. Prompt user for confirmation before saving changes.
    b. If confirmed, attempt to lock and update both account and customer records.
    c. If unable to lock records, set appropriate error flags and messages.
    d. If records are successfully updated, set ACUP-CHANGES-OKAYED-AND-DONE to TRUE.
    e. If update fails, perform a rollback and set appropriate error flags.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Update Account and Customer Records

- Detailed Description of the Rule:
  This rule handles the process of updating the account and customer records in the database after all validations have passed.

- What it proposes to do:
  The purpose is to safely update the account and customer information in the database, ensuring data integrity and handling any potential errors during the update process.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Begin a database transaction.
2. Attempt to lock the account record for update:
   a. If successful, proceed.
   b. If unsuccessful, set an error flag and prepare an error message.
3. Attempt to lock the customer record for update:
   a. If successful, proceed.
   b. If unsuccessful, set an error flag and prepare an error message.
4. If either lock attempt failed:
   a. Roll back the transaction.
   b. Display the error message to the user.
   c. Exit the update process.
5. If locks are successful, check if the records have been modified since they were last read:
   a. Compare each field in the database record with the original values stored in the program.
   b. If any field has changed:
      - Set a flag indicating data was changed.
      - Roll back the transaction.
      - Prepare a message informing the user that the data has changed.
      - Exit the update process.
6. If no changes detected, proceed with updates:
   a. Update the account record:
      - Update each field with the new values from the program variables.
      - Write the updated record back to the database.
   b. Update the customer record:
      - Update each field with the new values from the program variables.
      - Write the updated record back to the database.
7. If any database write operation fails:
   a. Set an error flag.
   b. Roll back the transaction.
   c. Prepare an error message for the user.
8. If all updates are successful:
   a. Commit the transaction.
   b. Prepare a success message for the user.
9. Release all record locks.
10. Display the appropriate message (success or error) to the user.
11. Specific fields to update for the account record:
    - ACCT-ID
    - ACCT-ACTIVE-STATUS
    - ACCT-CURR-BAL
    - ACCT-CREDIT-LIMIT
    - ACCT-CASH-CREDIT-LIMIT
    - ACCT-CURR-CYC-CREDIT
    - ACCT-CURR-CYC-DEBIT
    - ACCT-OPEN-DATE (in YYYY-MM-DD format)
    - ACCT-EXPIRAION-DATE (in YYYY-MM-DD format)
    - ACCT-REISSUE-DATE (in YYYY-MM-DD format)
    - ACCT-GROUP-ID
12. Specific fields to update for the customer record:
    - CUST-ID
    - CUST-FIRST-NAME
    - CUST-MIDDLE-NAME
    - CUST-LAST-NAME
    - CUST-ADDR-LINE-1
    - CUST-ADDR-LINE-2
    - CUST-ADDR-LINE-3
    - CUST-ADDR-STATE-CD
    - CUST-ADDR-COUNTRY-CD
    - CUST-ADDR-ZIP
    - CUST-PHONE-NUM-1 (in (XXX)XXX-XXXX format)
    - CUST-PHONE-NUM-2 (in (XXX)XXX-XXXX format)
    - CUST-SSN
    - CUST-GOVT-ISSUED-ID
    - CUST-DOB-YYYY-MM-DD (in YYYY-MM-DD format)
    - CUST-EFT-ACCOUNT-ID
    - CUST-PRI-CARD-HOLDER-IND
    - CUST-FICO-CREDIT-SCORE
13. Perform case-insensitive comparisons for text fields when checking for changes.
14. Use EXEC CICS REWRITE commands to update the account and customer records.
15. If the account update succeeds but the customer update fails, roll back both changes using EXEC CICS SYNCPOINT ROLLBACK.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Handle Screen Navigation

- Detailed Description of the Rule:
  This rule manages the navigation between different screens and functions within the application based on user actions and program flow.

- What it proposes to do:
  The purpose is to control the flow of the application, ensuring that users are directed to the appropriate screens or functions based on their inputs and the current state of the program.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Identify the current screen or function.
2. Capture the user's action (e.g., pressing a specific function key).
3. Based on the current screen and user action:
   a. If on the initial search screen and user enters an account number:
      - Validate the account number (must be a non-zero 11-digit number).
      - If valid, retrieve account and customer data from ACCTDAT and CUSTDAT files.
      - Display the account details screen.
   b. If on the account details screen:
      - If user presses PF3 (exit):
        * Return to the calling program or main menu.
      - If user presses PF5 (save changes):
        * Validate all inputs, including:
          - Account status (must be 'Y' or 'N')
          - Credit limit (must be a valid signed number with 2 decimal places)
          - Cash credit limit (must be a valid signed number with 2 decimal places)
          - Current balance (must be a valid signed number with 2 decimal places)
          - Current cycle credit (must be a valid signed number with 2 decimal places)
          - Current cycle debit (must be a valid signed number with 2 decimal places)
          - Open date, expiry date, reissue date (must be valid dates)
          - SSN (must be a valid 9-digit number, not 000, 666, or 900-999 for the first 3 digits)
          - Date of birth (must be a valid date)
          - FICO score (must be between 300 and 850)
          - Names (must contain only alphabets)
          - Address (validate state code and zip code combination)
          - Phone numbers (must be valid US phone numbers)
        * If valid, attempt to update the ACCTDAT and CUSTDAT records.
        * Display result message (success or error).
      - If user presses PF12 (cancel):
        * Redisplay the account details without changes.
   c. If on any screen and user presses PF1 (help):
      - Display the help screen for the current function.
   d. If on any screen and an unexpected action is taken:
      - Display an error message.
      - Redisplay the current screen.
4. After each action:
   a. Update the program state variables to reflect the current screen and context.
   b. Prepare the appropriate screen for display (e.g., populate fields, set field attributes).
5. If transitioning to a new program:
   a. Prepare the communication area with relevant data.
   b. Initiate the CICS XCTL command to the new program.
6. If returning to the calling program:
   a. Prepare the communication area with result data.
   b. Initiate the CICS RETURN command.
7. Handle specific scenarios:
   - If no changes are detected when saving, display a message.
   - If the account or customer record is locked by another user, display an error message.
   - If the record was changed by someone else before update, redisplay with the latest data.
8. Implement error handling:
   - Display specific error messages for each validation failure.
   - Handle CICS response codes for file operations (e.g., NOTFND, DUPREC).
9. Maintain audit trail:
   - Log all successful updates to the account and customer records.
```

This revised rule incorporates the specific details and logic from the COBOL program, including the exact validations performed, the file operations, and the handling of various scenarios and error conditions.

<!--rule-end-->