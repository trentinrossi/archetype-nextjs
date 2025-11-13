# COSGN00C.cbl: Signon Screen for the CardDemo Application

## Overview
This CICS COBOL program, COSGN00C, serves as the signon screen for the CardDemo application. It handles user authentication by validating user credentials against a user security file. The program manages the display and processing of the signon screen, including error handling and navigation to appropriate screens based on user type after successful authentication.

<!-- general-rule-start -->
## General Business Rules:

1. Initial Screen Display:
   - When the program is first invoked (EIBCALEN = 0), it displays the signon screen with empty fields.
   - The cursor is positioned at the User ID field.

2. User Input Processing:
   - The program processes user input when the Enter key is pressed.
   - It validates that both User ID and Password fields are not empty.
   - If either field is empty, an appropriate error message is displayed, and the cursor is positioned at the empty field.

3. User Authentication:
   - User credentials are validated against a user security file (USRSEC).
   - The User ID is used as the key to read the user record from the file.
   - The password entered by the user is compared with the password stored in the user record.

4. Authentication Outcomes:
   - Successful Authentication:
     - For admin users (CDEMO-USRTYP-ADMIN), the program transfers control to COADM01C.
     - For non-admin users, the program transfers control to COMEN01C.
   - Failed Authentication:
     - If the password is incorrect, an error message is displayed, and the cursor is positioned at the Password field.
     - If the User ID is not found, an error message is displayed, and the cursor is positioned at the User ID field.
   - For any other errors during the authentication process, an appropriate error message is displayed.

5. Navigation:
   - The PF3 key can be used to exit the application, displaying a thank you message.
   - Any other key press results in an invalid key message being displayed.

6. Screen Information:
   - The screen displays header information including the current date, time, application ID, and system ID.
   - These details are updated each time the screen is displayed.

7. Error Handling:
   - The program uses a flag (WS-ERR-FLG) to track error conditions.
   - Error messages are displayed in the ERRMSG field of the screen.

8. Data Transformation:
   - User ID and Password are converted to uppercase before processing.

9. Security:
   - User credentials are validated against a separate security file, enhancing the application's security.

10. Program Flow:
    - The program uses a commarea to pass information between different parts of the application, ensuring continuity of the user session.
<!-- general-rule-end -->

This program plays a crucial role in the CardDemo application by providing a secure entry point and directing users to appropriate sections based on their authentication and authorization levels.
## Dependencies

This program relies on several external dependencies, including copybooks for data structures, system-level copybooks for CICS functionality, and a user security file for authentication. These dependencies are crucial for the program's operation and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common data structures for the CardDemo application | Copybook | N/A | Contains shared data structures that may need to be updated or replaced |
| COSGN00 | Screen map for the signon screen | Copybook | N/A | Defines the screen layout, which may need to be converted to a modern UI |
| COTTL01Y | Contains title information for screens | Copybook | N/A | May need to be adapted for a new UI framework |
| CSDAT01Y | Date-related data structures | Copybook | N/A | Date handling may need to be updated for modern systems |
| CSMSG01Y | Common messages used in the application | Copybook | N/A | Messages may need to be centralized in a resource file |
| CSUSR01Y | User-related data structures | Copybook | N/A | User data structures may need to be updated for modern authentication systems |
| DFHAID | CICS standard AID keys definitions | System Copybook | CICS Library | May need to be replaced with modern event handling |
| DFHBMSCA | CICS BMS screen attribute definitions | System Copybook | CICS Library | Screen attributes may need to be translated to modern UI frameworks |
| USRSEC | User security file | File | N/A | Authentication may need to be migrated to a modern database or identity management system |
| COADM01C | Admin menu program | Program | N/A | Integration point that may need to be updated in modernization |
| COMEN01C | Main menu program | Program | N/A | Integration point that may need to be updated in modernization |

These dependencies highlight the interconnected nature of the COBOL application and the potential areas that will require attention during modernization, such as updating UI components, revising data structures, and potentially replacing CICS-specific functionality with modern alternatives.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COSGN00C program. The data is primarily derived from the working storage section and the copybooks included in the program.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | PIC X(08) | 8 | Name of the current program | Yes | 'COSGN00C' | Used for logging and display | Yes | Internal |
| WS-TRANID | Transaction ID | PIC X(04) | 4 | ID of the current transaction | Yes | 'CC00' | Used for CICS operations | Yes | Internal |
| WS-MESSAGE | Message | PIC X(80) | 80 | Holds messages for display | No | SPACES | Used for user feedback | Yes | Display |
| WS-USRSEC-FILE | User Security File | PIC X(08) | 8 | Name of the user security file | Yes | 'USRSEC' | Used for user authentication | Yes | Configuration |
| WS-ERR-FLG | Error Flag | PIC X(01) | 1 | Indicates presence of an error | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Control |
| WS-RESP-CD | Response Code | PIC S9(09) COMP | 4 | Holds CICS response codes | No | ZEROS | Used for error handling | Yes | Control |
| WS-REAS-CD | Reason Code | PIC S9(09) COMP | 4 | Holds CICS reason codes | No | ZEROS | Used for error handling | Yes | Control |
| WS-USER-ID | User ID | PIC X(08) | 8 | Holds the entered user ID | Yes | N/A | Used for authentication | Yes | Input |
| WS-USER-PWD | User Password | PIC X(08) | 8 | Holds the entered password | Yes | N/A | Used for authentication | Yes | Input |
| USERIDI | User ID Input | PIC X(8) | 8 | User ID input field on screen | Yes | N/A | From COSGN00 copybook | Yes | Input |
| PASSWDI | Password Input | PIC X(8) | 8 | Password input field on screen | Yes | N/A | From COSGN00 copybook | Yes | Input |
| ERRMSGO | Error Message Output | PIC X(78) | 78 | Error message display field | No | N/A | From COSGN00 copybook | Yes | Display |
| TITLE01O | Title 1 Output | PIC X(40) | 40 | First title line on screen | Yes | N/A | From COSGN00 copybook | Yes | Display |
| TITLE02O | Title 2 Output | PIC X(40) | 40 | Second title line on screen | Yes | N/A | From COSGN00 copybook | Yes | Display |
| CURDATEO | Current Date Output | PIC X(8) | 8 | Current date display field | Yes | N/A | From COSGN00 copybook | Yes | Display |
| CURTIMEO | Current Time Output | PIC X(8) | 8 | Current time display field | Yes | N/A | From COSGN00 copybook | Yes | Display |
| APPLIDO | Application ID Output | PIC X(8) | 8 | CICS application ID display | Yes | N/A | From COSGN00 copybook | Yes | Display |
| SYSIDO | System ID Output | PIC X(8) | 8 | CICS system ID display | Yes | N/A | From COSGN00 copybook | Yes | Display |
| CDEMO-USER-ID | CardDemo User ID | PIC X(8) | 8 | User ID for CardDemo app | Yes | N/A | From COCOM01Y copybook | Yes | Session |
| CDEMO-USER-TYPE | CardDemo User Type | PIC X(1) | 1 | User type for CardDemo app | Yes | N/A | From COCOM01Y copybook | Yes | Session |
| SEC-USER-DATA | Security User Data | N/A | N/A | Structure holding user security data | Yes | N/A | From CSUSR01Y copybook | Yes | Security |

Note: The exact structure of SEC-USER-DATA is not provided in the given code snippet, but it's referenced in the program and likely contains fields such as SEC-USR-PWD for the user's password.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Initialize Signon Screen

- Detailed Description of the Rule:
  This rule initializes the signon screen when the program is first invoked and handles subsequent user interactions.
- What it proposes to do:
  Set up the initial state of the signon screen, clear any previous data, position the cursor, and process user input based on the key pressed.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set ERR-FLG-OFF to TRUE.
2. Move SPACES to WS-MESSAGE and ERRMSGO of COSGN0AO.
3. Check if EIBCALEN (CICS Communication Area Length) is equal to 0.
4. If EIBCALEN is 0:
   a. Move LOW-VALUES to COSGN0AO (output area for the screen).
   b. Set the cursor position to the User ID field by moving -1 to USERIDL of COSGN0AI.
   c. Call the SEND-SIGNON-SCREEN subroutine to display the initial screen.
5. If EIBCALEN is not 0, evaluate EIBAID (attention identifier):
   a. If EIBAID is DFHENTER:
      - Perform PROCESS-ENTER-KEY subroutine.
   b. If EIBAID is DFHPF3:
      - Move CCDA-MSG-THANK-YOU to WS-MESSAGE.
      - Perform SEND-PLAIN-TEXT subroutine.
   c. For any other key:
      - Set WS-ERR-FLG to 'Y'.
      - Move CCDA-MSG-INVALID-KEY to WS-MESSAGE.
      - Perform SEND-SIGNON-SCREEN subroutine.
6. Return to CICS, passing WS-TRANID and CARDDEMO-COMMAREA.

## PROCESS-ENTER-KEY Subroutine:
1. Receive input from the map 'COSGN0A'.
2. Validate user input:
   a. If USERIDI is spaces or low-values:
      - Set WS-ERR-FLG to 'Y'.
      - Set WS-MESSAGE to 'Please enter User ID ...'.
      - Set cursor to USERIDL.
      - Perform SEND-SIGNON-SCREEN.
   b. If PASSWDI is spaces or low-values:
      - Set WS-ERR-FLG to 'Y'.
      - Set WS-MESSAGE to 'Please enter Password ...'.
      - Set cursor to PASSWDL.
      - Perform SEND-SIGNON-SCREEN.
3. Convert USERIDI and PASSWDI to uppercase and store in WS-USER-ID, CDEMO-USER-ID, and WS-USER-PWD.
4. If no errors, perform READ-USER-SEC-FILE subroutine.

## SEND-SIGNON-SCREEN Subroutine:
1. Perform POPULATE-HEADER-INFO subroutine.
2. Move WS-MESSAGE to ERRMSGO of COSGN0AO.
3. Send the map 'COSGN0A' to the screen, erasing the previous content and positioning the cursor.

## SEND-PLAIN-TEXT Subroutine:
1. Send WS-MESSAGE as plain text, erasing the screen and freeing the keyboard.
2. Return to CICS.

## POPULATE-HEADER-INFO Subroutine:
1. Get the current date and time.
2. Populate header fields: TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO.
3. Format and display the current date (MM/DD/YY) and time (HH:MM:SS).
4. Retrieve and display APPLID and SYSID.

## READ-USER-SEC-FILE Subroutine:
1. Read the USRSEC file using WS-USER-ID as the key.
2. Based on the response:
   a. If successful (RESP = 0):
      - Check if SEC-USR-PWD matches WS-USER-PWD.
      - If matched:
        * Populate CDEMO-FROM-TRANID, CDEMO-FROM-PROGRAM, CDEMO-USER-ID, CDEMO-USER-TYPE.
        * Set CDEMO-PGM-CONTEXT to ZEROS.
        * If CDEMO-USRTYP-ADMIN, transfer control to 'COADM01C'.
        * Otherwise, transfer control to 'COMEN01C'.
      - If not matched:
        * Set WS-MESSAGE to 'Wrong Password. Try again ...'.
        * Set cursor to PASSWDL.
        * Perform SEND-SIGNON-SCREEN.
   b. If user not found (RESP = 13):
      - Set WS-ERR-FLG to 'Y'.
      - Set WS-MESSAGE to 'User not found. Try again ...'.
      - Set cursor to USERIDL.
      - Perform SEND-SIGNON-SCREEN.
   c. For any other response:
      - Set WS-ERR-FLG to 'Y'.
      - Set WS-MESSAGE to 'Unable to verify the User ...'.
      - Set cursor to USERIDL.
      - Perform SEND-SIGNON-SCREEN.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Name: Process User Input

- Detailed Description of the Rule:
  This rule handles the processing of user input based on the key pressed and the data entered.
- What it proposes to do:
  Determine the appropriate action based on the function key used by the user and validate the user input.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize error flag (WS-ERR-FLG) to 'N' and clear WS-MESSAGE and ERRMSGO.

2. If EIBCALEN is 0 (first time entry):
   - Initialize COSGN0AO to LOW-VALUES.
   - Set cursor position to USERIDL field.
   - Call the SEND-SIGNON-SCREEN subroutine.

3. If EIBCALEN is not 0, evaluate the EIBAID (Attention Identifier) to determine which key was pressed:
   a. If EIBAID is DFHENTER (Enter key):
      - Call the PROCESS-ENTER-KEY subroutine.
   b. If EIBAID is DFHPF3 (PF3 key):
      - Move the thank you message (CCDA-MSG-THANK-YOU) to WS-MESSAGE.
      - Call the SEND-PLAIN-TEXT subroutine to display the message.
   c. For any other key:
      - Set WS-ERR-FLG to 'Y' to indicate an error.
      - Move the invalid key message (CCDA-MSG-INVALID-KEY) to WS-MESSAGE.
      - Call the SEND-SIGNON-SCREEN subroutine to redisplay the screen with the error message.

4. PROCESS-ENTER-KEY subroutine:
   a. Receive input from the map COSGN0A.
   b. Validate user input:
      - If USERIDI is spaces or LOW-VALUES:
        * Set WS-ERR-FLG to 'Y'.
        * Set WS-MESSAGE to 'Please enter User ID ...'.
        * Set cursor position to USERIDL field.
        * Call SEND-SIGNON-SCREEN subroutine.
      - If PASSWDI is spaces or LOW-VALUES:
        * Set WS-ERR-FLG to 'Y'.
        * Set WS-MESSAGE to 'Please enter Password ...'.
        * Set cursor position to PASSWDL field.
        * Call SEND-SIGNON-SCREEN subroutine.
   c. If input is valid:
      - Convert USERIDI and PASSWDI to uppercase and store in WS-USER-ID and WS-USER-PWD respectively.
      - Also store WS-USER-ID in CDEMO-USER-ID.
      - If no errors, call READ-USER-SEC-FILE subroutine.

5. READ-USER-SEC-FILE subroutine:
   a. Read the USRSEC file using WS-USER-ID as the key.
   b. Based on the response code (WS-RESP-CD):
      - If 0 (successful read):
        * If SEC-USR-PWD matches WS-USER-PWD:
          - Set CDEMO-FROM-TRANID to WS-TRANID (CC00).
          - Set CDEMO-FROM-PROGRAM to WS-PGMNAME (COSGN00C).
          - Set CDEMO-USER-ID to WS-USER-ID.
          - Set CDEMO-USER-TYPE to SEC-USR-TYPE.
          - Set CDEMO-PGM-CONTEXT to ZEROS.
          - If CDEMO-USRTYP-ADMIN is true, transfer control to program 'COADM01C'.
          - Otherwise, transfer control to program 'COMEN01C'.
        * If passwords don't match:
          - Set WS-MESSAGE to 'Wrong Password. Try again ...'.
          - Set cursor position to PASSWDL field.
          - Call SEND-SIGNON-SCREEN subroutine.
      - If 13 (record not found):
        * Set WS-ERR-FLG to 'Y'.
        * Set WS-MESSAGE to 'User not found. Try again ...'.
        * Set cursor position to USERIDL field.
        * Call SEND-SIGNON-SCREEN subroutine.
      - For any other response code:
        * Set WS-ERR-FLG to 'Y'.
        * Set WS-MESSAGE to 'Unable to verify the User ...'.
        * Set cursor position to USERIDL field.
        * Call SEND-SIGNON-SCREEN subroutine.

6. Always return to the transaction with CARDDEMO-COMMAREA and its length.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: Process Enter Key

- Detailed Description of the Rule:
  This rule processes the user input when the Enter key is pressed.
- What it proposes to do:
  Validate user input, initiate the authentication process, and route to the appropriate program based on user type.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Receive the map data using CICS RECEIVE MAP command for 'COSGN0A' map.
2. Validate user input:
   a. If USERIDI of COSGN0AI is spaces or low-values:
      - Set WS-ERR-FLG to 'Y'.
      - Move 'Please enter User ID ...' to WS-MESSAGE.
      - Set cursor position to USERIDL of COSGN0AI.
      - Call SEND-SIGNON-SCREEN subroutine.
   b. If PASSWDI of COSGN0AI is spaces or low-values:
      - Set WS-ERR-FLG to 'Y'.
      - Move 'Please enter Password ...' to WS-MESSAGE.
      - Set cursor position to PASSWDL of COSGN0AI.
      - Call SEND-SIGNON-SCREEN subroutine.
3. If both fields are not empty:
   a. Convert USERIDI and PASSWDI to uppercase using FUNCTION UPPER-CASE and store in WS-USER-ID and WS-USER-PWD respectively.
   b. Move WS-USER-ID to CDEMO-USER-ID.
   c. If WS-ERR-FLG is not 'Y', call the READ-USER-SEC-FILE subroutine.
4. In READ-USER-SEC-FILE subroutine:
   a. Read the USRSEC file using the WS-USER-ID as the key.
   b. Based on the response code (WS-RESP-CD):
      - If response is 0 (successful read):
        * Compare SEC-USR-PWD with WS-USER-PWD
        * If passwords match:
          - Set CDEMO-FROM-TRANID to WS-TRANID (CC00)
          - Set CDEMO-FROM-PROGRAM to WS-PGMNAME (COSGN00C)
          - Set CDEMO-USER-ID to WS-USER-ID
          - Set CDEMO-USER-TYPE to SEC-USR-TYPE
          - Set CDEMO-PGM-CONTEXT to ZEROS
          - If CDEMO-USRTYP-ADMIN is true, transfer control to 'COADM01C' program
          - Otherwise, transfer control to 'COMEN01C' program
        * If passwords don't match:
          - Set WS-MESSAGE to 'Wrong Password. Try again ...'
          - Set cursor position to PASSWDL of COSGN0AI
          - Call SEND-SIGNON-SCREEN subroutine
      - If response is 13 (record not found):
        * Set WS-ERR-FLG to 'Y'
        * Set WS-MESSAGE to 'User not found. Try again ...'
        * Set cursor position to USERIDL of COSGN0AI
        * Call SEND-SIGNON-SCREEN subroutine
      - For any other response:
        * Set WS-ERR-FLG to 'Y'
        * Set WS-MESSAGE to 'Unable to verify the User ...'
        * Set cursor position to USERIDL of COSGN0AI
        * Call SEND-SIGNON-SCREEN subroutine
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: Read User Security File

- Detailed Description of the Rule:
  This rule reads the user security file to authenticate the user and determine the appropriate action based on authentication results.
- What it proposes to do:
  Validate user credentials and route the user to the appropriate program based on their user type.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform a CICS READ operation on the WS-USRSEC-FILE using WS-USER-ID as the key.
2. Evaluate the response code (WS-RESP-CD) from the READ operation:
   a. If WS-RESP-CD is 0 (successful read):
      - Compare SEC-USR-PWD from the file with WS-USER-PWD.
      - If passwords match:
        * Set up CARDDEMO-COMMAREA with the following details:
          - CDEMO-FROM-TRANID = WS-TRANID (CC00)
          - CDEMO-FROM-PROGRAM = WS-PGMNAME (COSGN00C)
          - CDEMO-USER-ID = WS-USER-ID
          - CDEMO-USER-TYPE = SEC-USR-TYPE
          - CDEMO-PGM-CONTEXT = ZEROS
        * If CDEMO-USRTYP-ADMIN is true:
          - Transfer control to 'COADM01C' program with CARDDEMO-COMMAREA.
        * Else:
          - Transfer control to 'COMEN01C' program with CARDDEMO-COMMAREA.
      - If passwords don't match:
        * Move 'Wrong Password. Try again ...' to WS-MESSAGE.
        * Set cursor position to PASSWDL of COSGN0AI.
        * Call SEND-SIGNON-SCREEN subroutine.
   b. If WS-RESP-CD is 13 (record not found):
      - Set WS-ERR-FLG to 'Y'.
      - Move 'User not found. Try again ...' to WS-MESSAGE.
      - Set cursor position to USERIDL of COSGN0AI.
      - Call SEND-SIGNON-SCREEN subroutine.
   c. For any other response code:
      - Set WS-ERR-FLG to 'Y'.
      - Move 'Unable to verify the User ...' to WS-MESSAGE.
      - Set cursor position to USERIDL of COSGN0AI.
      - Call SEND-SIGNON-SCREEN subroutine.
3. If SEND-SIGNON-SCREEN is called:
   - Populate header information including current date and time.
   - Move WS-MESSAGE to ERRMSGO of COSGN0AO.
   - Send the COSGN0A map with ERASE and CURSOR options.
4. After processing, return to the calling program with:
   - TRANSID set to WS-TRANID (CC00)
   - COMMAREA set to CARDDEMO-COMMAREA
   - LENGTH set to the length of CARDDEMO-COMMAREA

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: Send Signon Screen

- Detailed Description of the Rule:
  This rule prepares and sends the signon screen to the user.
- What it proposes to do:
  Update the screen with current information and any error messages before displaying it.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Call the POPULATE-HEADER-INFO subroutine to update screen header information:
   - Set TITLE01O and TITLE02O from CCDA-TITLE01 and CCDA-TITLE02
   - Set TRNNAMEO to 'CC00'
   - Set PGMNAMEO to 'COSGN00C'
   - Set CURDATEO to current date in MM/DD/YY format
   - Set CURTIMEO to current time in HH:MM:SS format
   - Set APPLIDO to the current CICS application ID
   - Set SYSIDO to the current CICS system ID
2. Move WS-MESSAGE to ERRMSGO of COSGN0AO to display any error messages.
3. Use CICS SEND MAP command to send the COSGN0A map from the COSGN00 mapset.
4. Set the ERASE option to clear the screen before displaying the new content.
5. Set the CURSOR option to position the cursor as specified earlier.
6. If EIBCALEN is 0 (initial entry):
   - Move LOW-VALUES to COSGN0AO
   - Set cursor position to USERIDL field (-1)
7. Handle different scenarios based on EIBAID:
   - DFHENTER: Process the entered data
   - DFHPF3: Display "Thank you" message and exit
   - Other keys: Display "Invalid key pressed" error and redisplay the screen
8. For DFHENTER, validate user input:
   - If USERIDI is empty, display "Please enter User ID ..." error
   - If PASSWDI is empty, display "Please enter Password ..." error
   - If both fields are filled, convert them to uppercase and attempt to read the user security file
9. When reading the user security file:
   - If user is found and password matches:
     - Set CDEMO-FROM-TRANID to 'CC00'
     - Set CDEMO-FROM-PROGRAM to 'COSGN00C'
     - Set CDEMO-USER-ID to the entered user ID
     - Set CDEMO-USER-TYPE to the user type from the security file
     - Set CDEMO-PGM-CONTEXT to zeros
     - If user type is admin, transfer control to 'COADM01C' program
     - Otherwise, transfer control to 'COMEN01C' program
   - If user is found but password doesn't match, display "Wrong Password. Try again ..." error
   - If user is not found, display "User not found. Try again ..." error
   - For other errors, display "Unable to verify the User ..." error
10. Return to CICS with TRANSID 'CC00' and the CARDDEMO-COMMAREA
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Name: Send Plain Text

- Detailed Description of the Rule:
  This rule sends a plain text message to the screen and ends the transaction.
- What it proposes to do:
  Display a simple message (the content of WS-MESSAGE) and end the transaction.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Use CICS SEND TEXT command to send the content of WS-MESSAGE.
2. Set the LENGTH option to the length of WS-MESSAGE.
3. Set the ERASE option to clear the screen before displaying the message.
4. Set the FREEKB option to free the keyboard.
5. Use CICS RETURN command to end the transaction immediately after sending the text, without any additional processing.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
```markdown
- ## Rule / Function / Method - Name: Populate Header Info

- Detailed Description of the Rule:
  This rule populates the header information for the signon screen.
- What it proposes to do:
  Update the screen with current date, time, and system information.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Get the current date and time using FUNCTION CURRENT-DATE.
2. Move CCDA-TITLE01 to TITLE01O of COSGN0AO.
3. Move CCDA-TITLE02 to TITLE02O of COSGN0AO.
4. Move WS-TRANID to TRNNAMEO of COSGN0AO.
5. Move WS-PGMNAME to PGMNAMEO of COSGN0AO.
6. Format the current date:
   a. Extract month from WS-CURDATE-MONTH and move to WS-CURDATE-MM.
   b. Extract day from WS-CURDATE-DAY and move to WS-CURDATE-DD.
   c. Extract last two digits of year from WS-CURDATE-YEAR(3:2) and move to WS-CURDATE-YY.
   d. Move formatted date (WS-CURDATE-MM-DD-YY) to CURDATEO of COSGN0AO.
7. Format the current time:
   a. Move WS-CURTIME-HOURS to WS-CURTIME-HH.
   b. Move WS-CURTIME-MINUTE to WS-CURTIME-MM.
   c. Move WS-CURTIME-SECOND to WS-CURTIME-SS.
   d. Move formatted time (WS-CURTIME-HH-MM-SS) to CURTIMEO of COSGN0AO.
8. Use CICS ASSIGN command to get the APPLID and move it to APPLIDO of COSGN0AO.
9. Use CICS ASSIGN command to get the SYSID and move it to SYSIDO of COSGN0AO.
```

<!--rule-end-->