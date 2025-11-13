# COUSR01C.cbl: Add New User to USRSEC File

## Overview
This CICS COBOL program, COUSR01C, is part of the CardDemo application. Its primary function is to add a new Regular or Admin user to the USRSEC file. The program handles the user interface for inputting new user details and performs the necessary operations to add the user to the security file.

The file plays a crucial role in the user management aspect of the CardDemo system, allowing administrators to create new user accounts with appropriate access levels. It contributes to the overall functionality by maintaining the security and access control of the application.

<!-- general-rule-start -->
## General Business Rules:

1. User Input Validation:
   - The program validates that all required fields (First Name, Last Name, User ID, Password, and User Type) are not empty.
   - If any field is empty, an appropriate error message is displayed, and the user is prompted to fill in the missing information.

2. User ID Uniqueness:
   - The program checks if the entered User ID already exists in the USRSEC file.
   - If a duplicate User ID is found, an error message is displayed, and the user is asked to choose a different User ID.

3. User Data Storage:
   - Upon successful validation, the program writes the new user's information to the USRSEC file.
   - The user data includes First Name, Last Name, User ID, Password, and User Type.

4. Screen Navigation:
   - The program allows users to return to the previous screen (likely an admin menu) using the PF3 key.
   - The PF4 key is used to clear the current input screen.

5. Error Handling:
   - The program displays appropriate error messages for various scenarios, such as empty fields, duplicate User IDs, or system errors during the write operation.

6. Screen Layout:
   - The program maintains a consistent screen layout, including a header with the current date, time, transaction ID, and program name.

7. Data Security:
   - While the program handles user passwords, it does not implement any visible encryption or hashing mechanism in this code snippet. (Note: It's recommended to implement proper password hashing for security purposes.)

8. User Types:
   - The program allows for the creation of different user types, likely distinguishing between regular users and administrators.

9. Audit Trail:
   - The program doesn't explicitly implement an audit trail for user creation actions. (Note: Implementing an audit log for such security-related actions would be a good practice.)

10. Session Management:
    - The program uses a COMMAREA to maintain session information and allow navigation between different screens of the application.

This program is essential for maintaining the user base of the CardDemo application, ensuring that new users can be added to the system with proper validation and error handling.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies, including copybooks for data structures, system-level CICS interfaces, and a user security file. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COPY COCOM01Y. | Contains shared data structures; may need to be replaced or adapted in a modernized system |
| COUSR01 | User interface map for user addition | Copybook | COPY COUSR01. | Defines the screen layout; may need to be replaced with a modern UI framework |
| COTTL01Y | Title information | Copybook | COPY COTTL01Y. | Contains title information; may need to be adapted for a new UI |
| CSDAT01Y | Date handling routines | Copybook | COPY CSDAT01Y. | Date handling may need to be updated to more modern standards |
| CSMSG01Y | Common messages | Copybook | COPY CSMSG01Y. | Message handling may need to be centralized or internationalized |
| CSUSR01Y | User data structures | Copybook | COPY CSUSR01Y. | User data structures may need to be updated or normalized in a modern database |
| DFHAID | CICS AID constants | Copybook | COPY DFHAID. | CICS-specific; may need to be replaced with equivalent web technologies |
| DFHBMSCA | CICS BMS screen attributes | Copybook | COPY DFHBMSCA. | CICS-specific; may need to be replaced with modern UI frameworks |
| USRSEC | User security file | File | WS-USRSEC-FILE | May need to be replaced with a modern database system for user management |
| CICS | CICS runtime environment | System | N/A | Core dependency; modernization may involve moving away from CICS to a web-based architecture |

These dependencies highlight the program's reliance on CICS and traditional mainframe technologies. In a modernization effort, special attention would need to be given to replacing CICS-specific functionalities, updating the user interface, and potentially migrating the user security data to a more modern database system.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COUSR01C program, focusing on the fields relevant to user management and screen interaction.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | PIC X(08) | 8 | Stores the program name | Yes | 'COUSR01C' | Used for identification | Yes | System |
| WS-TRANID | Transaction ID | PIC X(04) | 4 | Stores the transaction ID | Yes | 'CU01' | Used for CICS transaction | Yes | System |
| WS-MESSAGE | Message | PIC X(80) | 80 | Stores error or success messages | No | SPACES | Displayed on screen | Yes | User Interface |
| WS-USRSEC-FILE | User Security File | PIC X(08) | 8 | Name of the user security file | Yes | 'USRSEC' | File for storing user data | Yes | Database |
| WS-ERR-FLG | Error Flag | PIC X(01) | 1 | Indicates presence of an error | No | 'N' | 'Y' for error, 'N' for no error | Yes | System |
| SEC-USR-ID | User ID | Unknown | Unknown | Stores the user's ID | Yes | None | Key field for user record | Yes | User Data |
| SEC-USR-FNAME | First Name | Unknown | Unknown | Stores the user's first name | Yes | None | Part of user record | Yes | User Data |
| SEC-USR-LNAME | Last Name | Unknown | Unknown | Stores the user's last name | Yes | None | Part of user record | Yes | User Data |
| SEC-USR-PWD | Password | Unknown | Unknown | Stores the user's password | Yes | None | Should be encrypted in modern systems | Yes | User Data |
| SEC-USR-TYPE | User Type | Unknown | Unknown | Stores the user's type (e.g., admin, regular) | Yes | None | Determines user permissions | Yes | User Data |
| FNAMEI | First Name Input | PIC X(20) | 20 | Input field for first name | Yes | None | From COUSR01 map | Yes | User Interface |
| LNAMEI | Last Name Input | PIC X(20) | 20 | Input field for last name | Yes | None | From COUSR01 map | Yes | User Interface |
| USERIDI | User ID Input | PIC X(8) | 8 | Input field for user ID | Yes | None | From COUSR01 map | Yes | User Interface |
| PASSWDI | Password Input | PIC X(8) | 8 | Input field for password | Yes | None | From COUSR01 map | Yes | User Interface |
| USRTYPEI | User Type Input | PIC X(1) | 1 | Input field for user type | Yes | None | From COUSR01 map | Yes | User Interface |
| ERRMSGO | Error Message Output | PIC X(78) | 78 | Field for displaying error messages | No | None | From COUSR01 map | Yes | User Interface |

Note: The exact sizes and types of the SEC-USR-* fields are not provided in the given code snippet. These would typically be defined in the CSUSR01Y copybook. In a modernized system, these fields might be mapped to a user entity or table in a relational database.



<!--rule-start-->
## Reviewed Rule 1
- ## Rule / Function / Method - Name: Process-Enter-Key

- Detailed Description of the Rule:
  This rule handles the processing of user input when the Enter key is pressed on the user addition screen. It validates the input fields and initiates the user addition process if all validations pass.

- What it proposes to do:
  Ensure all required fields are filled out correctly before attempting to add a new user to the system.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check if First Name (FNAMEI) is empty or contains only spaces or low-values:
   - If true, set error flag (WS-ERR-FLG) to 'Y'
   - Set error message (WS-MESSAGE) to 'First Name can NOT be empty...'
   - Set cursor position (FNAMEL) to -1
   - Perform SEND-USRADD-SCREEN

2. If First Name is not empty, check if Last Name (LNAMEI) is empty or contains only spaces or low-values:
   - If true, set error flag (WS-ERR-FLG) to 'Y'
   - Set error message (WS-MESSAGE) to 'Last Name can NOT be empty...'
   - Set cursor position (LNAMEL) to -1
   - Perform SEND-USRADD-SCREEN

3. If Last Name is not empty, check if User ID (USERIDI) is empty or contains only spaces or low-values:
   - If true, set error flag (WS-ERR-FLG) to 'Y'
   - Set error message (WS-MESSAGE) to 'User ID can NOT be empty...'
   - Set cursor position (USERIDL) to -1
   - Perform SEND-USRADD-SCREEN

4. If User ID is not empty, check if Password (PASSWDI) is empty or contains only spaces or low-values:
   - If true, set error flag (WS-ERR-FLG) to 'Y'
   - Set error message (WS-MESSAGE) to 'Password can NOT be empty...'
   - Set cursor position (PASSWDL) to -1
   - Perform SEND-USRADD-SCREEN

5. If Password is not empty, check if User Type (USRTYPEI) is empty or contains only spaces or low-values:
   - If true, set error flag (WS-ERR-FLG) to 'Y'
   - Set error message (WS-MESSAGE) to 'User Type can NOT be empty...'
   - Set cursor position (USRTYPEL) to -1
   - Perform SEND-USRADD-SCREEN

6. If all fields are filled:
   - Set cursor position (FNAMEL) to -1

7. If error flag (WS-ERR-FLG) is not 'Y' (using the condition NOT ERR-FLG-ON):
   - Move USERIDI to SEC-USR-ID
   - Move FNAMEI to SEC-USR-FNAME
   - Move LNAMEI to SEC-USR-LNAME
   - Move PASSWDI to SEC-USR-PWD
   - Move USRTYPEI to SEC-USR-TYPE
   - Perform WRITE-USER-SEC-FILE

8. WRITE-USER-SEC-FILE procedure:
   - Attempt to write the user data to the USRSEC file
   - If successful (RESP is DFHRESP(NORMAL)):
     - Perform INITIALIZE-ALL-FIELDS
     - Set WS-MESSAGE to spaces
     - Set ERRMSGC of COUSR1AO to DFHGREEN
     - Set WS-MESSAGE to "User [SEC-USR-ID] has been added ..."
     - Perform SEND-USRADD-SCREEN
   - If duplicate key or record (RESP is DFHRESP(DUPKEY) or DFHRESP(DUPREC)):
     - Set WS-ERR-FLG to 'Y'
     - Set WS-MESSAGE to 'User ID already exist...'
     - Set cursor position (USERIDL) to -1
     - Perform SEND-USRADD-SCREEN
   - For any other response:
     - Set WS-ERR-FLG to 'Y'
     - Set WS-MESSAGE to 'Unable to Add User...'
     - Set cursor position (FNAMEL) to -1
     - Perform SEND-USRADD-SCREEN

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Name: Write-User-Sec-File

- Detailed Description of the Rule:
  This rule handles the process of writing a new user's information to the USRSEC file. It attempts to write the data and handles various response scenarios, including data validation and error handling.

- What it proposes to do:
  Add a new user record to the USRSEC file after validating input fields, and provide appropriate feedback based on the operation's success or failure.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Validate input fields:
   a. Check if First Name is empty:
      - If empty, set error flag, set message to "First Name can NOT be empty...", set cursor to FNAMEL, and send screen
   b. Check if Last Name is empty:
      - If empty, set error flag, set message to "Last Name can NOT be empty...", set cursor to LNAMEL, and send screen
   c. Check if User ID is empty:
      - If empty, set error flag, set message to "User ID can NOT be empty...", set cursor to USERIDL, and send screen
   d. Check if Password is empty:
      - If empty, set error flag, set message to "Password can NOT be empty...", set cursor to PASSWDL, and send screen
   e. Check if User Type is empty:
      - If empty, set error flag, set message to "User Type can NOT be empty...", set cursor to USRTYPEL, and send screen

2. If all fields are valid, prepare data for writing:
   - Move USERIDI to SEC-USR-ID
   - Move FNAMEI to SEC-USR-FNAME
   - Move LNAMEI to SEC-USR-LNAME
   - Move PASSWDI to SEC-USR-PWD
   - Move USRTYPEI to SEC-USR-TYPE

3. Execute CICS WRITE operation:
   - Set DATASET to WS-USRSEC-FILE (value 'USRSEC  ')
   - Set FROM to SEC-USER-DATA
   - Set LENGTH to the length of SEC-USER-DATA
   - Set RIDFLD to SEC-USR-ID
   - Set KEYLENGTH to the length of SEC-USR-ID
   - Store response code in WS-RESP-CD
   - Store reason code in WS-REAS-CD

4. Evaluate the response code (WS-RESP-CD):
   a. If response is NORMAL:
      - Perform INITIALIZE-ALL-FIELDS
      - Set WS-MESSAGE to spaces
      - Set ERRMSGC of COUSR1AO to DFHGREEN
      - Construct success message: "User [SEC-USR-ID] has been added ..."
      - Perform SEND-USRADD-SCREEN

   b. If response is DUPKEY or DUPREC:
      - Set WS-ERR-FLG to 'Y'
      - Set WS-MESSAGE to 'User ID already exist...'
      - Set cursor position (USERIDL) to -1
      - Perform SEND-USRADD-SCREEN

   c. For any other response:
      - Set WS-ERR-FLG to 'Y'
      - Set WS-MESSAGE to 'Unable to Add User...'
      - Set cursor position (FNAMEL) to -1
      - Perform SEND-USRADD-SCREEN

5. INITIALIZE-ALL-FIELDS:
   - Set cursor position (FNAMEL) to -1
   - Clear USERIDI, FNAMEI, LNAMEI, PASSWDI, USRTYPEI, and WS-MESSAGE

Note: This algorithm includes data validation before writing to the file. The modernized system should consider adding additional data sanitization or encryption (especially for the password) before storing the user data.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: Clear-Current-Screen

- Detailed Description of the Rule:
  This rule clears all input fields on the current screen, resets the display, and positions the cursor to the first input field.

- What it proposes to do:
  Provide a way for users to clear all entered data and start over with a fresh input screen, with the cursor positioned at the beginning.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform INITIALIZE-ALL-FIELDS
   - Set cursor position (FNAMEL) to -1
   - Clear USERIDI, FNAMEI, LNAMEI, PASSWDI, USRTYPEI fields to SPACES
   - Clear WS-MESSAGE to SPACES

2. Perform SEND-USRADD-SCREEN
   - This will redisplay the screen with cleared fields and the cursor positioned at the first input field (FNAMEL)
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: Populate-Header-Info

- Detailed Description of the Rule:
  This rule populates the header information of the screen, including the current date and time.

- What it proposes to do:
  Ensure that each displayed screen shows up-to-date information in the header.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Get the current date and time:
   - Move FUNCTION CURRENT-DATE to WS-CURDATE-DATA

2. Set screen titles:
   - Move CCDA-TITLE01 to TITLE01O of COUSR1AO
   - Move CCDA-TITLE02 to TITLE02O of COUSR1AO

3. Set transaction and program names:
   - Move WS-TRANID to TRNNAMEO of COUSR1AO
   - Move WS-PGMNAME to PGMNAMEO of COUSR1AO

4. Format and set the current date:
   - Move WS-CURDATE-MONTH to WS-CURDATE-MM
   - Move WS-CURDATE-DAY to WS-CURDATE-DD
   - Move the last 2 digits of WS-CURDATE-YEAR to WS-CURDATE-YY
   - Move formatted date (WS-CURDATE-MM-DD-YY) to CURDATEO of COUSR1AO

5. Format and set the current time:
   - Move WS-CURTIME-HOURS to WS-CURTIME-HH
   - Move WS-CURTIME-MINUTE to WS-CURTIME-MM
   - Move WS-CURTIME-SECOND to WS-CURTIME-SS
   - Move formatted time (WS-CURTIME-HH-MM-SS) to CURTIMEO of COUSR1AO

Note: The exact format of the date and time fields is as follows:
- WS-CURDATE-DATA is the result of FUNCTION CURRENT-DATE
- WS-CURDATE-MONTH, WS-CURDATE-DAY, and WS-CURDATE-YEAR are parts of WS-CURDATE-DATA
- WS-CURTIME-HOURS, WS-CURTIME-MINUTE, and WS-CURTIME-SECOND are parts of WS-CURDATE-DATA
- The date is formatted as MM-DD-YY
- The time is formatted as HH-MM-SS

<!--rule-end-->