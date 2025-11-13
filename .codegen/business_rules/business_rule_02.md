# COMEN01C.cbl: Main Menu for Regular Users

## Overview
This COBOL program, COMEN01C, is part of the CardDemo application and functions as the main menu for regular users. It is a CICS COBOL program that presents a menu of options to the user, processes their selection, and routes them to the appropriate program based on their choice. The program also handles user authentication and access control, ensuring that regular users cannot access admin-only options.

<!-- general-rule-start -->
## General Business Rules:

1. User Authentication:
   - The program checks if the user is authenticated by verifying the EIBCALEN (COMMAREA length).
   - If EIBCALEN is 0, it means the user is not authenticated, and the program returns to the sign-on screen (COSGN00C).

2. Menu Display:
   - The program displays a menu of options for regular users.
   - The menu options are dynamically built based on the user's access level (regular user or admin).
   - Each option is numbered and includes a description.

3. Option Selection:
   - Users can select an option by entering its corresponding number.
   - The program validates the entered option to ensure it's numeric and within the valid range.

4. Access Control:
   - The program checks the user's type (regular or admin) before processing the selected option.
   - If a regular user attempts to access an admin-only option, an error message is displayed.

5. Navigation:
   - Users can navigate to different parts of the application based on their selection.
   - The program uses CICS XCTL to transfer control to the selected program.

6. Error Handling:
   - The program displays appropriate error messages for invalid inputs or unauthorized access attempts.

7. Screen Management:
   - The program manages the display of the menu screen, including populating header information and building menu options.

8. Session Management:
   - The program maintains session information using the COMMAREA, allowing for seamless navigation between different parts of the application.

9. Exit Functionality:
   - Users can exit the menu and return to the sign-on screen by pressing PF3.

10. Date and Time Display:
    - The current date and time are displayed on the menu screen, formatted as MM/DD/YY and HH:MM:SS respectively.

This program serves as a central hub for regular users in the CardDemo application, providing a user-friendly interface to access various functions while enforcing proper access control and maintaining session continuity.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies, including copybooks for data structures, common routines, and CICS-specific definitions. These dependencies are crucial for the program's functionality and will need to be considered during any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common data structures | Copybook | COCOM01Y | Contains shared data structures that may need to be updated or replaced |
| COMEN02Y | Menu-specific data | Copybook | COMEN02Y | Contains menu-related data structures that may need to be adapted for a new UI |
| COMEN01 | Screen layout for main menu | Copybook | COMEN01 | Defines the screen layout, which may need to be replaced with a modern UI framework |
| COTTL01Y | Title information | Copybook | COTTL01Y | Contains title information that may need to be adapted for a new UI |
| CSDAT01Y | Date-related data | Copybook | CSDAT01Y | Contains date-related structures that may need to be updated for modern date handling |
| CSMSG01Y | Common messages | Copybook | CSMSG01Y | Contains message definitions that may need to be centralized in a message service |
| CSUSR01Y | User-related data | Copybook | CSUSR01Y | Contains user-related structures that may need to be adapted for modern user management |
| DFHAID | CICS AID definitions | Copybook | DFHAID | CICS-specific definitions that may need to be replaced in a non-CICS environment |
| DFHBMSCA | CICS BMS screen attributes | Copybook | DFHBMSCA | CICS-specific screen attributes that may need to be replaced with modern UI controls |
| USRSEC | User security file | File | USRSEC | External file for user security, may need to be replaced with a modern authentication system |
| COSGN00C | Sign-on program | Program | COSGN00C | Related program for user sign-on, may need to be modernized alongside this program |

These dependencies highlight the interconnected nature of the COBOL application and the potential challenges in modernization. The copybooks contain crucial data structures and definitions that may need to be transformed or replaced in a modern architecture. The CICS-specific dependencies (DFHAID, DFHBMSCA) indicate a strong coupling with the CICS environment, which would require significant changes if moving to a different platform. The external file (USRSEC) and related program (COSGN00C) suggest that user authentication and security would be key areas to address in any modernization effort.
## Detailed Rules
## Data Structure

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | PIC X | 8 | Stores the current program name | Yes | 'COMEN01C' | Used for logging and navigation | Yes | Internal |
| WS-TRANID | Transaction ID | PIC X | 4 | Stores the CICS transaction ID | Yes | 'CM00' | Used for CICS transaction management | Yes | Internal |
| WS-MESSAGE | Message | PIC X | 80 | Stores messages to be displayed to the user | No | SPACES | Used for user feedback | Yes | Display |
| WS-USRSEC-FILE | User Security File | PIC X | 8 | Name of the user security file | Yes | 'USRSEC' | Used for user authentication | Yes | Configuration |
| WS-ERR-FLG | Error Flag | PIC X | 1 | Indicates if an error has occurred | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Internal |
| WS-RESP-CD | Response Code | PIC S9(09) COMP | 4 bytes | Stores CICS response codes | No | ZEROS | Used for error handling | Yes | Internal |
| WS-REAS-CD | Reason Code | PIC S9(09) COMP | 4 bytes | Stores CICS reason codes | No | ZEROS | Used for detailed error information | Yes | Internal |
| WS-OPTION-X | Option (Alphanumeric) | PIC X | 2 | Stores user's menu selection as text | No | N/A | Intermediate storage for input processing | Yes | Internal |
| WS-OPTION | Option (Numeric) | PIC 9 | 2 | Stores user's menu selection as number | No | 0 | Used to determine selected menu option | Yes | Internal |
| WS-IDX | Index | PIC S9(04) COMP | 2 bytes | Used as a loop counter | No | ZEROS | Used in various loops for menu processing | Yes | Internal |
| WS-MENU-OPT-TXT | Menu Option Text | PIC X | 40 | Stores formatted menu option text | No | SPACES | Used to build menu display | Yes | Internal |
| CDEMO-FROM-PROGRAM | From Program | PIC X | 8 | Stores the name of the calling program | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Session |
| CDEMO-TO-PROGRAM | To Program | PIC X | 8 | Stores the name of the program to transfer to | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Session |
| CDEMO-PGM-REENTER | Program Reenter Flag | PIC X | 1 | Indicates if the program is being reentered | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Session |
| CDEMO-USRTYP-USER | User Type Flag | PIC X | 1 | Indicates if the user is a regular user | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Session |
| CDEMO-MENU-OPT-COUNT | Menu Option Count | PIC 9 | 2 | Stores the number of menu options | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Configuration |
| CDEMO-MENU-OPT-USRTYPE | Menu Option User Type | PIC X | 1 | Stores the user type required for each menu option | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Configuration |
| CDEMO-MENU-OPT-PGMNAME | Menu Option Program Name | PIC X | 8 | Stores the program name for each menu option | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Configuration |
| CDEMO-MENU-OPT-NAME | Menu Option Name | PIC X | 40 | Stores the display name for each menu option | Yes | N/A | Part of CARDDEMO-COMMAREA | Yes | Display |
| TRNNAMEO | Transaction Name Output | PIC X | 4 | Displays the transaction name on the screen | Yes | N/A | Part of COMEN1AO | Yes | Display |
| TITLE01O | Title 1 Output | PIC X | 40 | Displays the first title line on the screen | Yes | N/A | Part of COMEN1AO | Yes | Display |
| TITLE02O | Title 2 Output | PIC X | 40 | Displays the second title line on the screen | Yes | N/A | Part of COMEN1AO | Yes | Display |
| CURDATEO | Current Date Output | PIC X | 8 | Displays the current date on the screen | Yes | N/A | Part of COMEN1AO | Yes | Display |
| CURTIMEO | Current Time Output | PIC X | 8 | Displays the current time on the screen | Yes | N/A | Part of COMEN1AO | Yes | Display |
| OPTIONO | Selected Option Output | PIC X | 2 | Displays the user's selected option | No | N/A | Part of COMEN1AO | Yes | Display |
| ERRMSGO | Error Message Output | PIC X | 78 | Displays error messages on the screen | No | N/A | Part of COMEN1AO | Yes | Display |



<!--rule-start-->
## Reviewed Rule 1
- ## Rule / Function / Method - Name: MAIN-PARA

- Detailed Description of the Rule:
  This is the main control flow of the program. It handles the initial setup, determines whether to display the menu or process user input, and manages the program's overall execution.

- What it proposes to do:
  Coordinate the program's flow, manage error flags, and direct the execution to appropriate subroutines based on the program's state and user actions.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize the error flag (WS-ERR-FLG) to 'N' (off).
2. Clear the message fields (WS-MESSAGE and ERRMSGO of COMEN1AO).
3. Check if EIBCALEN (CICS COMMAREA length) is 0:
   - If true, set CDEMO-FROM-PROGRAM to 'COSGN00C' and perform RETURN-TO-SIGNON-SCREEN.
   - If false, continue to step 4.
4. Move the contents of DFHCOMMAREA to CARDDEMO-COMMAREA.
5. Check if CDEMO-PGM-REENTER is false:
   - If true:
     a. Set CDEMO-PGM-REENTER to true.
     b. Initialize COMEN1AO to LOW-VALUES.
     c. Perform SEND-MENU-SCREEN.
   - If false, continue to step 6.
6. Perform RECEIVE-MENU-SCREEN.
7. Evaluate EIBAID (attention identifier):
   - If DFHENTER:
     Perform PROCESS-ENTER-KEY.
   - If DFHPF3:
     a. Set CDEMO-TO-PROGRAM to 'COSGN00C'.
     b. Perform RETURN-TO-SIGNON-SCREEN.
   - For any other key:
     a. Set WS-ERR-FLG to 'Y'.
     b. Set WS-MESSAGE to CCDA-MSG-INVALID-KEY.
     c. Perform SEND-MENU-SCREEN.
8. Execute CICS RETURN with TRANSID set to WS-TRANID and COMMAREA set to CARDDEMO-COMMAREA.

## Additional Details:
- The program uses WS-TRANID with a value of 'CM00'.
- The program name (WS-PGMNAME) is 'COMEN01C'.
- The error flag (WS-ERR-FLG) is set using level-88 condition names: ERR-FLG-ON for 'Y' and ERR-FLG-OFF for 'N'.
- The program includes various copybooks: COCOM01Y, COMEN02Y, COMEN01, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y, DFHAID, and DFHBMSCA.
- The LINKAGE SECTION includes DFHCOMMAREA with a variable length structure LK-COMMAREA.
- The SEND-MENU-SCREEN procedure populates header information and builds menu options before sending the screen.
- The RECEIVE-MENU-SCREEN procedure receives user input from the screen.
- The PROCESS-ENTER-KEY procedure handles user menu selections, including input validation and access control based on user type.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - Name: PROCESS-ENTER-KEY

- Detailed Description of the Rule:
  This subroutine processes the user's menu selection when the Enter key is pressed. It validates the input, checks user permissions, and either executes the selected option or displays an error message.

- What it proposes to do:
  Validate user input, enforce access control, and route the user to the appropriate program based on their selection.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Determine the length of the user's input in OPTIONI of COMEN1AI:
   a. Start from the rightmost character and move left until a non-space character is found or the beginning is reached.
   b. Store this length in WS-IDX.
2. Move the user's input (trimmed of trailing spaces) to WS-OPTION-X.
3. Replace all spaces in WS-OPTION-X with '0'.
4. Convert WS-OPTION-X to a numeric value and store in WS-OPTION.
5. Move WS-OPTION to OPTIONO of COMEN1AO.
6. Validate the user's input:
   - If WS-OPTION is not numeric, or
   - If WS-OPTION is greater than CDEMO-MENU-OPT-COUNT, or
   - If WS-OPTION is zero:
     a. Set WS-ERR-FLG to 'Y'.
     b. Set WS-MESSAGE to 'Please enter a valid option number...'.
     c. Perform SEND-MENU-SCREEN.
     d. Exit the subroutine.
7. Check user permissions:
   - If CDEMO-USRTYP-USER is true and CDEMO-MENU-OPT-USRTYPE(WS-OPTION) is 'A':
     a. Set ERR-FLG-ON to true.
     b. Set WS-MESSAGE to 'No access - Admin Only option... '.
     c. Perform SEND-MENU-SCREEN.
     d. Exit the subroutine.
8. If no errors (ERR-FLG-ON is false):
   - If the first 5 characters of CDEMO-MENU-OPT-PGMNAME(WS-OPTION) are not 'DUMMY':
     a. Set CDEMO-FROM-TRANID to WS-TRANID.
     b. Set CDEMO-FROM-PROGRAM to WS-PGMNAME.
     c. Set CDEMO-PGM-CONTEXT to zeros.
     d. Execute CICS XCTL to the program specified in CDEMO-MENU-OPT-PGMNAME(WS-OPTION), passing CARDDEMO-COMMAREA.
   - Else:
     a. Clear WS-MESSAGE.
     b. Set ERRMSGC of COMEN1AO to DFHGREEN.
     c. Construct a message in WS-MESSAGE: "This option [option name] is coming soon ..."
     d. Perform SEND-MENU-SCREEN.

Note: The rule now accurately reflects all conditions and actions present in the COBOL source code, including the check for ERR-FLG-ON before processing valid options.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: RETURN-TO-SIGNON-SCREEN

- Detailed Description of the Rule:
  This subroutine handles the return to the sign-on screen, ensuring that the correct program is called for the sign-on process.

- What it proposes to do:
  Manage the transition from the main menu back to the sign-on screen, maintaining program flow and data integrity.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check if CDEMO-TO-PROGRAM is LOW-VALUES or SPACES:
   - If true, set CDEMO-TO-PROGRAM to 'COSGN00C'.
2. Execute CICS XCTL to the program specified in CDEMO-TO-PROGRAM.
3. This subroutine does not return control to the calling program, as CICS XCTL transfers control to the specified program.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: SEND-MENU-SCREEN

- Detailed Description of the Rule:
  This subroutine prepares and sends the menu screen to the user. It populates the screen with header information, builds the menu options, and displays any relevant messages.

- What it proposes to do:
  Construct and display the main menu interface, ensuring all dynamic elements are properly populated.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform POPULATE-HEADER-INFO subroutine:
   - Move current date and time to WS-CURDATE-DATA
   - Set TITLE01O and TITLE02O from CCDA-TITLE01 and CCDA-TITLE02
   - Set TRNNAMEO to WS-TRANID (CM00)
   - Set PGMNAMEO to WS-PGMNAME (COMEN01C)
   - Format and set CURDATEO to current date (MM/DD/YY)
   - Format and set CURTIMEO to current time (HH:MM:SS)

2. Perform BUILD-MENU-OPTIONS subroutine:
   - Iterate through CDEMO-MENU-OPT-COUNT (up to 12 options)
   - For each option:
     - Construct WS-MENU-OPT-TXT with option number and name
     - Move WS-MENU-OPT-TXT to corresponding OPTNnnnO field (001 to 012)

3. Move WS-MESSAGE to ERRMSGO of COMEN1AO.

4. Execute CICS SEND command with the following parameters:
   - MAP: 'COMEN1A'
   - MAPSET: 'COMEN01'
   - FROM: COMEN1AO
   - ERASE: Yes

5. Note: This subroutine is part of a larger program flow:
   - It's called after setting CDEMO-PGM-REENTER to TRUE and moving LOW-VALUES to COMEN1AO on first entry
   - It's also called after processing user input (ENTER key, PF3 key, or invalid key)
   - The screen can display error messages (e.g., invalid option, no access) or informational messages (e.g., "coming soon")

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: RECEIVE-MENU-SCREEN

- Detailed Description of the Rule:
  This subroutine receives user input from the menu screen, storing it in the program's internal data structures for processing.

- What it proposes to do:
  Capture user input from the CICS screen and make it available for the program to process.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS RECEIVE command with the following parameters:
   - MAP: 'COMEN1A'
   - MAPSET: 'COMEN01'
   - INTO: COMEN1AI
   - RESP: WS-RESP-CD
   - RESP2: WS-REAS-CD
2. Store the response code in WS-RESP-CD.
3. Store the reason code in WS-REAS-CD.
4. The received data is stored in the COMEN1AI structure, which contains the user input from the menu screen.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Name: POPULATE-HEADER-INFO

- Detailed Description of the Rule:
  This subroutine populates the header information for the menu screen, including titles, current date, time, transaction ID, and program name.

- What it proposes to do:
  Prepare and format the header information to be displayed on the menu screen.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Get the current date and time using FUNCTION CURRENT-DATE and store in WS-CURDATE-DATA.
2. Move CCDA-TITLE01 to TITLE01O of COMEN1AO.
3. Move CCDA-TITLE02 to TITLE02O of COMEN1AO.
4. Move WS-TRANID to TRNNAMEO of COMEN1AO.
5. Move WS-PGMNAME to PGMNAMEO of COMEN1AO.
6. Format the current date:
   a. Move WS-CURDATE-MONTH to WS-CURDATE-MM.
   b. Move WS-CURDATE-DAY to WS-CURDATE-DD.
   c. Move the last two digits of WS-CURDATE-YEAR (positions 3-4) to WS-CURDATE-YY.
   d. Move the formatted date (WS-CURDATE-MM-DD-YY) to CURDATEO of COMEN1AO.
7. Format the current time:
   a. Move WS-CURTIME-HOURS to WS-CURTIME-HH.
   b. Move WS-CURTIME-MINUTE to WS-CURTIME-MM.
   c. Move WS-CURTIME-SECOND to WS-CURTIME-SS.
   d. Move the formatted time (WS-CURTIME-HH-MM-SS) to CURTIMEO of COMEN1AO.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
```markdown
- ## Rule / Function / Method - Name: BUILD-MENU-OPTIONS

- Detailed Description of the Rule:
  This subroutine constructs the menu options to be displayed on the screen, formatting each option with its number and description.

- What it proposes to do:
  Dynamically generate the menu options based on the available functions defined in the CDEMO-MENU-OPT tables.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize WS-IDX to 1.
2. Perform the following steps for each menu option (repeat until WS-IDX > CDEMO-MENU-OPT-COUNT):
   a. Clear WS-MENU-OPT-TXT by setting it to SPACES.
   b. Construct the menu option text in WS-MENU-OPT-TXT:
      - Start with CDEMO-MENU-OPT-NUM(WS-IDX).
      - Append '. ' (period followed by a space).
      - Append CDEMO-MENU-OPT-NAME(WS-IDX).
   c. Based on the value of WS-IDX, move WS-MENU-OPT-TXT to the corresponding output field:
      - If WS-IDX is 1, move to OPTN001O
      - If WS-IDX is 2, move to OPTN002O
      - If WS-IDX is 3, move to OPTN003O
      - If WS-IDX is 4, move to OPTN004O
      - If WS-IDX is 5, move to OPTN005O
      - If WS-IDX is 6, move to OPTN006O
      - If WS-IDX is 7, move to OPTN007O
      - If WS-IDX is 8, move to OPTN008O
      - If WS-IDX is 9, move to OPTN009O
      - If WS-IDX is 10, move to OPTN010O
      - If WS-IDX is 11, move to OPTN011O
      - If WS-IDX is 12, move to OPTN012O
      - For any other value of WS-IDX, continue to the next iteration without moving the text
   d. Increment WS-IDX by 1.
3. End the loop when all options have been processed (WS-IDX > CDEMO-MENU-OPT-COUNT).
```

<!--rule-end-->