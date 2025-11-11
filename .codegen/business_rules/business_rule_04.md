# COUSR00C.cbl: User List Management

## Overview
This COBOL program, COUSR00C, is part of the CardDemo application and functions as a CICS transaction to list all users from the USRSEC file. It provides a paginated view of user records, allowing navigation through the list and options to update or delete user information.

The program's main purpose is to display a list of users, handle user interactions for navigating through the list, and provide options for user management tasks. It contributes to the overall functionality of the system by offering a user interface for administrators to view and manage user accounts.

<!-- general-rule-start -->
## General Business Rules:

1. User List Display:
   - The program displays a list of users from the USRSEC file.
   - It shows up to 10 user records per page.
   - For each user, it displays the user ID, first name, last name, and user type.

2. Navigation:
   - Users can navigate through the list using PF7 (backward) and PF8 (forward) keys.
   - The program keeps track of the current page number and updates it accordingly.
   - It handles reaching the top or bottom of the list, displaying appropriate messages.

3. User Selection:
   - Administrators can select a user record for further actions (update or delete).
   - Selection is done by entering 'U' or 'D' next to the user ID.

4. User Management Options:
   - When a user is selected for update ('U'), the program transfers control to COUSR02C.
   - When a user is selected for deletion ('D'), the program transfers control to COUSR03C.

5. Search Functionality:
   - The program allows searching for users by entering a partial or full user ID.
   - The search is case-sensitive and starts from the entered user ID.

6. Error Handling:
   - The program handles various error scenarios, such as file access errors or invalid user inputs.
   - It displays appropriate error messages on the screen.

7. Screen Management:
   - The program manages the display of the user list screen, including header information and error messages.
   - It handles the receiving and sending of screen data.

8. File Operations:
   - The program performs various operations on the USRSEC file, including starting a browse, reading records forward and backward, and ending the browse.

9. Program Flow:
   - The program follows a specific flow based on user actions and system responses.
   - It uses different sub-routines to handle various aspects of the functionality, such as processing different key presses and populating user data.

10. Integration:
    - The program integrates with other parts of the CardDemo application, using a common area (CARDDEMO-COMMAREA) for data transfer between programs.

This program plays a crucial role in user management within the CardDemo application, providing administrators with the necessary tools to view and manage user accounts efficiently.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies to function properly within the CardDemo application. These dependencies include copybooks for data structures, system interfaces, and file definitions. Understanding these dependencies is crucial for any modernization effort, as they define the program's interaction with other system components and data structures.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COCOM01Y | Contains shared data structures used across multiple programs. May need to be updated or replaced with a more modern data sharing mechanism. |
| COUSR00 | User list screen map | Copybook | COUSR00 | Defines the screen layout for the user list. In a web-based modernization, this would be replaced with HTML/CSS. |
| COTTL01Y | Screen title definitions | Copybook | COTTL01Y | Contains common screen titles. May be replaced with a configuration file or database in a modernized system. |
| CSDAT01Y | Date handling routines | Copybook | CSDAT01Y | Date manipulation logic. Consider replacing with standard library functions in the target language. |
| CSMSG01Y | Common messages | Copybook | CSMSG01Y | Defines common messages. Could be moved to a resource file or database in a modernized system. |
| CSUSR01Y | User data structures | Copybook | CSUSR01Y | Defines user-related data structures. May need to be adapted to work with a modern database system. |
| DFHAID | CICS AID constants | Copybook | DFHAID | CICS-specific constants. Will need to be replaced or simulated in a non-CICS environment. |
| DFHBMSCA | CICS BMS screen attributes | Copybook | DFHBMSCA | CICS-specific screen attributes. Will need to be replaced with appropriate UI framework in modernization. |
| USRSEC | User security file | File | USRSEC | External file storing user data. Will likely be replaced with a database in a modernized system. |

These dependencies play a crucial role in the program's functionality and will require careful consideration during any modernization effort. The copybooks define important data structures and screen layouts, while the USRSEC file represents the persistent storage of user data. In a modernized system, many of these dependencies might be replaced with more contemporary alternatives, such as databases for data storage, web technologies for user interfaces, and standard libraries for common functionalities like date handling.
## Detailed Rules
## Data Structure

This section details the key data structures used in the COUSR00C program. These structures are crucial for managing user information and screen interactions.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-VARIABLES.WS-PGMNAME | WS-PGMNAME | PIC X(08) | 8 | Program name | Yes | 'COUSR00C' | Used for identification | Yes | Internal |
| WS-VARIABLES.WS-TRANID | WS-TRANID | PIC X(04) | 4 | Transaction ID | Yes | 'CU00' | Used for CICS transactions | Yes | Internal |
| WS-VARIABLES.WS-MESSAGE | WS-MESSAGE | PIC X(80) | 80 | Message for display | No | SPACES | Used for user feedback | Yes | Display |
| WS-VARIABLES.WS-USRSEC-FILE | WS-USRSEC-FILE | PIC X(08) | 8 | User security file name | Yes | 'USRSEC' | File containing user data | Yes | Internal |
| WS-VARIABLES.WS-ERR-FLG | WS-ERR-FLG | PIC X(01) | 1 | Error flag | Yes | 'N' | Indicates error state | Yes | Internal |
| WS-VARIABLES.WS-USER-SEC-EOF | WS-USER-SEC-EOF | PIC X(01) | 1 | End of file indicator | Yes | 'N' | Indicates end of user file | Yes | Internal |
| WS-VARIABLES.WS-SEND-ERASE-FLG | WS-SEND-ERASE-FLG | PIC X(01) | 1 | Screen erase flag | Yes | 'Y' | Controls screen clearing | Yes | Internal |
| WS-VARIABLES.WS-RESP-CD | WS-RESP-CD | PIC S9(09) COMP | 4 | Response code | Yes | ZEROS | Stores CICS response codes | Yes | Internal |
| WS-VARIABLES.WS-REAS-CD | WS-REAS-CD | PIC S9(09) COMP | 4 | Reason code | Yes | ZEROS | Stores CICS reason codes | Yes | Internal |
| WS-VARIABLES.WS-REC-COUNT | WS-REC-COUNT | PIC S9(04) COMP | 2 | Record count | Yes | ZEROS | Counts processed records | Yes | Internal |
| WS-VARIABLES.WS-IDX | WS-IDX | PIC S9(04) COMP | 2 | Index variable | Yes | ZEROS | Used in loops | Yes | Internal |
| WS-VARIABLES.WS-PAGE-NUM | WS-PAGE-NUM | PIC S9(04) COMP | 2 | Page number | Yes | ZEROS | Tracks current page | Yes | Internal |
| WS-USER-DATA.USER-REC.USER-SEL | USER-SEL | PIC X(01) | 1 | User selection flag | No | N/A | For user selection | Yes | Input |
| WS-USER-DATA.USER-REC.USER-ID | USER-ID | PIC X(08) | 8 | User ID | Yes | N/A | Unique user identifier | Yes | Display/Input |
| WS-USER-DATA.USER-REC.USER-NAME | USER-NAME | PIC X(25) | 25 | User name | Yes | N/A | Full name of user | Yes | Display |
| WS-USER-DATA.USER-REC.USER-TYPE | USER-TYPE | PIC X(08) | 8 | User type | Yes | N/A | Type/role of user | Yes | Display |
| CDEMO-CU00-INFO.CDEMO-CU00-USRID-FIRST | CDEMO-CU00-USRID-FIRST | PIC X(08) | 8 | First user ID on page | No | N/A | For pagination | Yes | Internal |
| CDEMO-CU00-INFO.CDEMO-CU00-USRID-LAST | CDEMO-CU00-USRID-LAST | PIC X(08) | 8 | Last user ID on page | No | N/A | For pagination | Yes | Internal |
| CDEMO-CU00-INFO.CDEMO-CU00-PAGE-NUM | CDEMO-CU00-PAGE-NUM | PIC 9(08) | 8 | Current page number | Yes | N/A | For pagination | Yes | Display/Internal |
| CDEMO-CU00-INFO.CDEMO-CU00-NEXT-PAGE-FLG | CDEMO-CU00-NEXT-PAGE-FLG | PIC X(01) | 1 | Next page flag | Yes | 'N' | Indicates more pages | Yes | Internal |
| CDEMO-CU00-INFO.CDEMO-CU00-USR-SEL-FLG | CDEMO-CU00-USR-SEL-FLG | PIC X(01) | 1 | User selection flag | No | N/A | Indicates user selection | Yes | Internal |
| CDEMO-CU00-INFO.CDEMO-CU00-USR-SELECTED | CDEMO-CU00-USR-SELECTED | PIC X(08) | 8 | Selected user ID | No | N/A | Stores selected user ID | Yes | Internal |

Note: The table includes key fields from the working storage section and the CDEMO-CU00-INFO structure. The actual program may contain additional fields from copybooks (COUSR00, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y) which are not detailed here due to space constraints. These fields are crucial for the program's functionality and would need to be considered in any modernization effort.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Process-Enter-Key:
   - Detailed Description of the Rule:
     This rule handles the processing when the Enter key is pressed on the user list screen.
   - What it proposes to do:
     It determines if a user has been selected for update or delete and initiates the appropriate action. It also handles pagination and user search functionality.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check each of the 10 possible selection fields (SEL0001I to SEL0010I) in order.
   2. If a selection field is not empty and not low-values:
      a. Store the selection value in CDEMO-CU00-USR-SEL-FLG.
      b. Store the corresponding user ID in CDEMO-CU00-USR-SELECTED.
   3. If a user has been selected (CDEMO-CU00-USR-SEL-FLG is not spaces and not low-values) and (CDEMO-CU00-USR-SELECTED is not spaces and not low-values):
      a. If the selection is 'U' or 'u':
         - Set CDEMO-TO-PROGRAM to 'COUSR02C'.
         - Set CDEMO-FROM-TRANID to the current transaction ID (WS-TRANID).
         - Set CDEMO-FROM-PROGRAM to the current program name (WS-PGMNAME).
         - Set CDEMO-PGM-CONTEXT to 0.
         - Transfer control to COUSR02C program with CARDDEMO-COMMAREA.
      b. If the selection is 'D' or 'd':
         - Set CDEMO-TO-PROGRAM to 'COUSR03C'.
         - Set CDEMO-FROM-TRANID to the current transaction ID (WS-TRANID).
         - Set CDEMO-FROM-PROGRAM to the current program name (WS-PGMNAME).
         - Set CDEMO-PGM-CONTEXT to 0.
         - Transfer control to COUSR03C program with CARDDEMO-COMMAREA.
      c. If the selection is neither 'U', 'u', 'D', nor 'd':
         - Set WS-MESSAGE to 'Invalid selection. Valid values are U and D'.
         - Set the cursor position to USRIDINL field.
   4. If USRIDINI is spaces or low-values:
      - Set SEC-USR-ID to low-values.
   5. Otherwise:
      - Move USRIDINI to SEC-USR-ID.
   6. Set the cursor position to USRIDINL field.
   7. Set CDEMO-CU00-PAGE-NUM to 0.
   8. Perform the Process-Page-Forward routine:
      a. Start browsing the USRSEC file.
      b. Read up to 10 user records, populating the screen fields (USRID01I to USRID10I, FNAME01I to FNAME10I, LNAME01I to LNAME10I, UTYPE01I to UTYPE10I).
      c. Set CDEMO-CU00-USRID-FIRST to the first user ID read.
      d. Set CDEMO-CU00-USRID-LAST to the last user ID read.
      e. Increment CDEMO-CU00-PAGE-NUM if at least one record is read.
      f. Set NEXT-PAGE-YES if there are more records, otherwise set NEXT-PAGE-NO.
      g. End browsing the USRSEC file.
   9. If no error occurred:
      - Clear the USRIDINO field.
   10. Display the updated user list screen, showing the message in WS-MESSAGE if any.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Process-PF7-Key:
   - Detailed Description of the Rule:
     This rule handles the processing when the PF7 key (page up) is pressed on the user list screen.
   - What it proposes to do:
     It navigates to the previous page of user records if available.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. If CDEMO-CU00-USRID-FIRST is empty or low-values:
      - Set SEC-USR-ID to low-values.
   2. Otherwise:
      - Move CDEMO-CU00-USRID-FIRST to SEC-USR-ID.
   3. Set NEXT-PAGE-YES flag to true.
   4. Set the cursor position to USRIDINL field.
   5. If CDEMO-CU00-PAGE-NUM is greater than 1:
      - Perform the Process-Page-Backward routine:
        a. Start browsing the USRSEC file.
        b. If not at the start of file and no errors:
           - Initialize user data for all 10 rows.
           - Read previous records, populating user data from row 10 to 1.
           - If not at start of file and no errors after reading:
             - If NEXT-PAGE-YES is true:
               - If not at start of file, no errors, and CDEMO-CU00-PAGE-NUM > 1:
                 - Subtract 1 from CDEMO-CU00-PAGE-NUM.
               - Else:
                 - Set CDEMO-CU00-PAGE-NUM to 1.
           - End browsing the USRSEC file.
           - Move CDEMO-CU00-PAGE-NUM to PAGENUMI of COUSR0AI.
           - Send the updated USRLST screen.
   6. Otherwise:
      - Set WS-MESSAGE to 'You are already at the top of the page...'.
      - Set SEND-ERASE-NO flag to true.
      - Perform the Send-USRLST-Screen routine:
        a. Populate header info (current date, time, transaction ID, program name).
        b. Move WS-MESSAGE to ERRMSGO of COUSR0AO.
        c. Send the COUSR0A map without erasing the screen.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Process-PF8-Key:
   - Detailed Description of the Rule:
     This rule handles the processing when the PF8 key (page down) is pressed on the user list screen.
   - What it proposes to do:
     It navigates to the next page of user records if available.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. If CDEMO-CU00-USRID-LAST is empty or low-values:
      - Set SEC-USR-ID to high-values.
   2. Otherwise:
      - Move CDEMO-CU00-USRID-LAST to SEC-USR-ID.
   3. Set the cursor position to USRIDINL field.
   4. If NEXT-PAGE-YES flag is true:
      - Perform the Process-Page-Forward routine:
        a. Start browsing the USER-SEC file.
        b. If not the first time (EIBAID not DFHENTER, DFHPF7, or DFHPF3), read the next record.
        c. Initialize user data for all 10 rows.
        d. Read up to 10 records, populating user data for each row.
        e. If there are more records after the 10th:
           - Increment CDEMO-CU00-PAGE-NUM.
           - Set NEXT-PAGE-YES flag.
        f. If no more records:
           - Set NEXT-PAGE-NO flag.
           - If at least one record was read, increment CDEMO-CU00-PAGE-NUM.
        g. End browsing the USER-SEC file.
        h. Move CDEMO-CU00-PAGE-NUM to PAGENUMI of COUSR0AI.
        i. Clear USRIDINO of COUSR0AO.
        j. Send the updated USRLST screen.
   5. Otherwise (if NEXT-PAGE-YES flag is false):
      - Set WS-MESSAGE to 'You are already at the bottom of the page...'.
      - Set SEND-ERASE-NO flag to true.
      - Perform the Send-USRLST-Screen routine:
        a. Populate header information (date, time, transaction ID, program name).
        b. Move WS-MESSAGE to ERRMSGO of COUSR0AO.
        c. Send the COUSR0A map without erasing the screen.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Process-Page-Forward:
   - Detailed Description of the Rule:
     This rule handles the process of moving forward through the user records.
   - What it proposes to do:
     It retrieves the next set of user records and updates the screen display.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform the StartBR-User-Sec-File routine.
   2. If no error occurred:
      a. If the action is not Enter, PF7, or PF3:
         - Perform the ReadNext-User-Sec-File routine.
      b. If not end of file and no error:
         - Initialize user data for all 10 display slots.
      c. Set WS-IDX to 1.
      d. Perform the following until WS-IDX >= 11 or end of file or error occurs:
         - Perform the ReadNext-User-Sec-File routine.
         - If not end of file and no error:
           * Perform the Populate-User-Data routine:
             - Populate user data fields (ID, First Name, Last Name, Type) for the current index.
             - If WS-IDX is 1, set CDEMO-CU00-USRID-FIRST to the current user ID.
             - If WS-IDX is 10, set CDEMO-CU00-USRID-LAST to the current user ID.
           * Increment WS-IDX by 1.
      e. If not end of file and no error:
         - Increment CDEMO-CU00-PAGE-NUM by 1.
         - Perform the ReadNext-User-Sec-File routine.
         - If not end of file and no error:
           * Set NEXT-PAGE-YES flag to true.
         - Else:
           * Set NEXT-PAGE-NO flag to true.
      f. Else:
         - Set NEXT-PAGE-NO flag to true.
         - If WS-IDX > 1:
           * Increment CDEMO-CU00-PAGE-NUM by 1.
      g. Perform the EndBR-User-Sec-File routine.
      h. Move CDEMO-CU00-PAGE-NUM to PAGENUMI of COUSR0AI.
      i. Clear USRIDINO of COUSR0AO.
      j. Perform the Send-USRLST-Screen routine:
         - Populate header information including current date and time.
         - Move any error message to ERRMSGO of COUSR0AO.
         - Send the screen with ERASE option if SEND-ERASE-YES is true, otherwise send without ERASE.
   3. If an error occurred at any point:
      - Set the error flag.
      - Set an appropriate error message.
      - Move -1 to USRIDINL of COUSR0AI.
      - Perform the Send-USRLST-Screen routine.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Process-Page-Backward:
   - Detailed Description of the Rule:
     This rule handles the process of moving backward through the user records.
   - What it proposes to do:
     It retrieves the previous set of user records and updates the screen display.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform the StartBR-User-Sec-File routine.
      - If an error occurs (RESP not NORMAL or NOTFND), set ERR-FLG-ON, display an error message, and send the USRLST screen.
      - If NOTFND, set USER-SEC-EOF to TRUE and display "You are at the top of the page..." message.
   2. If no error occurred:
      a. If the action is not Enter or PF8:
         - Perform the ReadPrev-User-Sec-File routine.
      b. If not end of file and no error:
         - Initialize user data for all 10 display slots (USRID01I to USRID10I, FNAME01I to FNAME10I, LNAME01I to LNAME10I, UTYPE01I to UTYPE10I).
      c. Set WS-IDX to 10.
      d. Perform the following until WS-IDX <= 0 or end of file or error occurs:
         - Perform the ReadPrev-User-Sec-File routine.
         - If not end of file and no error:
           * Perform the Populate-User-Data routine, which moves SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, and SEC-USR-TYPE to the appropriate fields in COUSR0AI based on WS-IDX.
           * Decrement WS-IDX by 1.
      e. If not end of file and no error:
         - Perform the ReadPrev-User-Sec-File routine.
         - If NEXT-PAGE-YES flag is true:
           * If not end of file, no error, and CDEMO-CU00-PAGE-NUM > 1:
             - Subtract 1 from CDEMO-CU00-PAGE-NUM.
           * Else:
             - Set CDEMO-CU00-PAGE-NUM to 1.
      f. Perform the EndBR-User-Sec-File routine.
      g. Move CDEMO-CU00-PAGE-NUM to PAGENUMI of COUSR0AI.
      h. Perform the Send-USRLST-Screen routine.
         - This includes populating header info with current date and time, and displaying any error messages.

   ## Error Handling
   - If an error occurs during file operations, set ERR-FLG-ON, display "Unable to lookup User..." message, and send the USRLST screen.
   - If end of file is reached during ReadPrev, set USER-SEC-EOF to TRUE, display "You have reached the top of the page..." message, and send the USRLST screen.

   ## Additional Notes
   - The routine uses a STARTBR command with GTEQ (greater than or equal) positioning.
   - The USRSEC file is used for user security data.
   - The cursor is positioned at the USRIDIN field (USRIDINL) after each screen send.
   - The routine maintains CDEMO-CU00-USRID-FIRST and CDEMO-CU00-USRID-LAST for tracking the first and last user IDs on the current page.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Populate-User-Data:
   - Detailed Description of the Rule:
     This rule populates the user data fields on the screen with information from the current record based on the index (WS-IDX) value.
   - What it proposes to do:
     It maps the user record fields from the SEC-USER-DATA structure to the corresponding screen fields in COUSR0AI based on the current index.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Based on the value of WS-IDX (1 to 10), do the following:
      - Move SEC-USR-ID to USRIDxxI of COUSR0AI (where xx is the index number, e.g., USRID01I for index 1).
      - Move SEC-USR-FNAME to FNAMExxI of COUSR0AI.
      - Move SEC-USR-LNAME to LNAMExxI of COUSR0AI.
      - Move SEC-USR-TYPE to UTYPExxI of COUSR0AI.
   2. If WS-IDX is 1:
      - Also move SEC-USR-ID to CDEMO-CU00-USRID-FIRST.
   3. If WS-IDX is 10:
      - Also move SEC-USR-ID to CDEMO-CU00-USRID-LAST.
   4. The rule uses an EVALUATE statement to determine which set of screen fields to populate based on WS-IDX.
   5. The rule handles all 10 possible index values (1 through 10) individually.
   6. For each index value, it moves the corresponding SEC-USER-DATA fields to the appropriate COUSR0AI fields.
   7. This process is part of a larger routine that reads through user records and populates the screen with up to 10 user entries at a time.
   8. The rule is called repeatedly as part of a loop that processes multiple user records until the screen is filled or there are no more records.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Initialize-User-Data:
   - Detailed Description of the Rule:
     This rule initializes the user data fields on the screen to spaces based on the current index (WS-IDX).
   - What it proposes to do:
     It clears the user record fields on the screen for a specific row index (1 to 10).

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Based on the value of WS-IDX (1 to 10), do the following:
      - Move spaces to USRIDxxI of COUSR0AI (where xx is the index number, 01 to 10).
      - Move spaces to FNAMExxI of COUSR0AI.
      - Move spaces to LNAMExxI of COUSR0AI.
      - Move spaces to UTYPExxI of COUSR0AI.
   2. The specific fields to be initialized for each index are:
      - Index 1: USRID01I, FNAME01I, LNAME01I, UTYPE01I
      - Index 2: USRID02I, FNAME02I, LNAME02I, UTYPE02I
      - Index 3: USRID03I, FNAME03I, LNAME03I, UTYPE03I
      - Index 4: USRID04I, FNAME04I, LNAME04I, UTYPE04I
      - Index 5: USRID05I, FNAME05I, LNAME05I, UTYPE05I
      - Index 6: USRID06I, FNAME06I, LNAME06I, UTYPE06I
      - Index 7: USRID07I, FNAME07I, LNAME07I, UTYPE07I
      - Index 8: USRID08I, FNAME08I, LNAME08I, UTYPE08I
      - Index 9: USRID09I, FNAME09I, LNAME09I, UTYPE09I
      - Index 10: USRID10I, FNAME10I, LNAME10I, UTYPE10I
   3. This initialization is performed as part of the process to prepare for displaying a new page of user data.
   4. The initialization is typically done in a loop, iterating 10 times to clear all rows before populating with new data.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Send-USRLST-Screen:
   - Detailed Description of the Rule:
     This rule sends the updated user list screen to the terminal, populating it with user data from the USRSEC file.
   - What it proposes to do:
     It populates the header information, user list data, and sends the screen, either with erase or without based on the flag.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform the Populate-Header-Info routine:
      - Set current date and time
      - Set transaction ID (CU00) and program name (COUSR00C)
      - Set title information
   2. Move WS-MESSAGE to ERRMSGO of COUSR0AO.
   3. If SEND-ERASE-YES flag is true:
      - Send the map 'COUSR0A' from COUSR0AO with erase and cursor positioning.
   4. Otherwise:
      - Send the map 'COUSR0A' from COUSR0AO without erase but with cursor positioning.
   5. The screen includes the following information:
      - Header with current date, time, transaction ID, and program name
      - Error message (if any)
      - List of up to 10 users with the following fields for each:
        - Selection field (1 character)
        - User ID (8 characters)
        - First Name (25 characters)
        - Last Name (25 characters)
        - User Type (8 characters)
      - Page number
      - Input field for user ID search
   6. The screen supports the following function keys:
      - ENTER: Process the entered data or selection
      - PF3: Return to the previous screen (COADM01C)
      - PF7: Display the previous page of users
      - PF8: Display the next page of users
   7. For user selection, valid values are 'U' (update) and 'D' (delete), which will transfer control to COUSR02C or COUSR03C respectively.
   8. The screen maintains the first and last user ID displayed for pagination purposes.
   9. If there are more users to display, set the NEXT-PAGE-YES flag; otherwise, set NEXT-PAGE-NO.
   10. Update the page number (CDEMO-CU00-PAGE-NUM) when navigating pages.
   11. Handle error conditions:
       - Display appropriate error messages for invalid selections or system errors
       - Set the cursor position to the user ID input field (-1 to USRIDINL) when errors occur
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 9
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Populate-Header-Info:
   - Detailed Description of the Rule:
     This rule populates the header information on the screen.
   - What it proposes to do:
     It sets up the title, transaction name, program name, current date, and time on the screen.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Get the current date and time using FUNCTION CURRENT-DATE.
   2. Move CCDA-TITLE01 to TITLE01O of COUSR0AO.
   3. Move CCDA-TITLE02 to TITLE02O of COUSR0AO.
   4. Move WS-TRANID to TRNNAMEO of COUSR0AO.
   5. Move WS-PGMNAME to PGMNAMEO of COUSR0AO.
   6. Format the current date as MM/DD/YY:
      - Extract month from WS-CURDATE-MONTH and move to WS-CURDATE-MM.
      - Extract day from WS-CURDATE-DAY and move to WS-CURDATE-DD.
      - Extract last two digits of year from WS-CURDATE-YEAR(3:2) and move to WS-CURDATE-YY.
      - Combine WS-CURDATE-MM-DD-YY and move to CURDATEO of COUSR0AO.
   7. Format the current time as HH:MM:SS:
      - Extract hours from WS-CURTIME-HOURS and move to WS-CURTIME-HH.
      - Extract minutes from WS-CURTIME-MINUTE and move to WS-CURTIME-MM.
      - Extract seconds from WS-CURTIME-SECOND and move to WS-CURTIME-SS.
      - Combine WS-CURTIME-HH-MM-SS and move to CURTIMEO of COUSR0AO.
```

<!--rule-end-->