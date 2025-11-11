# API Documentation - Credit Card and Account Management System (COCRDLIC)

## Overview

This document provides a comprehensive summary of all REST API endpoints generated for the Credit Card and Account Management System. The system implements the COCRDLIC (Credit Card List) program with full business rule enforcement.

**Application**: COCRDLIC - Credit Card List Program  
**Framework**: Spring Boot 3.5.5 with Java 21  
**Database**: PostgreSQL with Flyway migrations  
**Architecture**: Layered architecture (Controller → Service → Repository → Entity)

---

## Business Rules Implemented

The API implements the following business rules:

- **BR001**: User Permission Based Card Access - Admin users can view all credit cards without context, while regular users can only view cards associated with their specific account
- **BR002**: Card Number Filter Validation - Card number must be numeric and exactly 16 digits if supplied, cannot be blank, spaces, or zeros
- **BR003**: Single Selection Enforcement - Users can only select one credit card at a time for view or update operations
- **BR004**: Account Filter Validation - Account ID must be numeric and exactly 11 digits if supplied, cannot be blank, spaces, or zeros
- **BR005**: Card Status Filter Validation - Card status must be a valid single character code
- **BR006**: Filter Record Matching - Records must match all supplied filter criteria to be displayed
- **BR008**: First Page Navigation Restriction - Users cannot navigate to previous pages when on the first page
- **BR009**: Last Page Navigation Restriction - Users cannot navigate to next pages when on the last page
- **BR011**: Exit to Menu - When user presses PF3, return to main menu program (COMEN01C)
- **BR012**: View Card Details - When user selects 'S' for a card, navigate to card detail view program (COCRDSLC)
- **BR013**: Update Card Information - When user selects 'U' for a card, navigate to card update program (COCRDUPC)
- **BR014**: Forward Pagination - Read next set of records when navigating forward through pages
- **BR015**: Backward Pagination - Read previous set of records when navigating backward through pages
- **BR017**: Input Error Protection - Protect row selection fields when input validation errors occur

---

## API Endpoints

### 1. Account Management API

**Base Path**: `/api/accounts`

#### 1.1 Get All Accounts
- **Method**: `GET`
- **Path**: `/api/accounts`
- **Description**: Retrieve a paginated list of all accounts
- **Query Parameters**:
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort criteria
- **Response**: `Page<AccountResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `400 Bad Request`: Invalid parameters
  - `500 Internal Server Error`: Server error

#### 1.2 Get Account by ID
- **Method**: `GET`
- **Path**: `/api/accounts/{accountId}`
- **Description**: Retrieve a specific account by its 11-digit ID
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
- **Response**: `AccountResponseDto`
- **Status Codes**:
  - `200 OK`: Account found
  - `404 Not Found`: Account not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR004

#### 1.3 Create Account
- **Method**: `POST`
- **Path**: `/api/accounts`
- **Description**: Create a new customer account
- **Request Body**: `CreateAccountRequestDto`
  ```json
  {
    "accountId": "12345678901"
  }
  ```
- **Response**: `AccountResponseDto`
- **Status Codes**:
  - `201 Created`: Account created successfully
  - `400 Bad Request`: Invalid data (account ID must be 11 digits)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR004

#### 1.4 Update Account
- **Method**: `PUT`
- **Path**: `/api/accounts/{accountId}`
- **Description**: Update account details
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
- **Request Body**: `UpdateAccountRequestDto`
  ```json
  {
    "accountId": "12345678901"
  }
  ```
- **Response**: `AccountResponseDto`
- **Status Codes**:
  - `200 OK`: Account updated successfully
  - `400 Bad Request`: Invalid data
  - `404 Not Found`: Account not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR004

#### 1.5 Delete Account
- **Method**: `DELETE`
- **Path**: `/api/accounts/{accountId}`
- **Description**: Delete an account by ID
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
- **Response**: No content
- **Status Codes**:
  - `204 No Content`: Account deleted successfully
  - `404 Not Found`: Account not found
  - `500 Internal Server Error`: Server error

#### 1.6 Get Accounts Accessible by User
- **Method**: `GET`
- **Path**: `/api/accounts/user/{userId}`
- **Description**: Retrieve accounts accessible by a specific user (admin users see all, regular users see only their accessible accounts)
- **Path Parameters**:
  - `userId` (required): User ID
- **Query Parameters**:
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
- **Response**: `Page<AccountResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 1.7 Grant User Access to Account
- **Method**: `POST`
- **Path**: `/api/accounts/{accountId}/grant-access/{userId}`
- **Description**: Grant a specific user access to an account
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
  - `userId` (required): User ID
- **Response**: No content
- **Status Codes**:
  - `200 OK`: Access granted successfully
  - `404 Not Found`: Account or user not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 1.8 Revoke User Access from Account
- **Method**: `POST`
- **Path**: `/api/accounts/{accountId}/revoke-access/{userId}`
- **Description**: Revoke a specific user's access to an account
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
  - `userId` (required): User ID
- **Response**: No content
- **Status Codes**:
  - `200 OK`: Access revoked successfully
  - `404 Not Found`: Account or user not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 1.9 Check User Access to Account
- **Method**: `GET`
- **Path**: `/api/accounts/{accountId}/has-access/{userId}`
- **Description**: Check if a user has access to a specific account
- **Path Parameters**:
  - `accountId` (required): 11-digit numeric account ID
  - `userId` (required): User ID
- **Response**: `Boolean`
- **Status Codes**:
  - `200 OK`: Check completed
  - `404 Not Found`: Account or user not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

---

### 2. User Management API

**Base Path**: `/api/users`

#### 2.1 Get All Users
- **Method**: `GET`
- **Path**: `/api/users`
- **Description**: Retrieve a paginated list of all users
- **Query Parameters**:
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort criteria
- **Response**: `Page<UserResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `400 Bad Request`: Invalid parameters
  - `500 Internal Server Error`: Server error

#### 2.2 Get User by ID
- **Method**: `GET`
- **Path**: `/api/users/{userId}`
- **Description**: Retrieve a specific user by their ID
- **Path Parameters**:
  - `userId` (required): User ID (alphanumeric, max 20 characters)
- **Response**: `UserResponseDto`
- **Status Codes**:
  - `200 OK`: User found
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

#### 2.3 Create User
- **Method**: `POST`
- **Path**: `/api/users`
- **Description**: Create a new system user
- **Request Body**: `CreateUserRequestDto`
  ```json
  {
    "userId": "USER001",
    "userType": "ADMIN"
  }
  ```
- **Response**: `UserResponseDto`
- **Status Codes**:
  - `201 Created`: User created successfully
  - `400 Bad Request`: Invalid data (user type must be ADMIN or REGULAR)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.4 Update User
- **Method**: `PUT`
- **Path**: `/api/users/{userId}`
- **Description**: Update user details
- **Path Parameters**:
  - `userId` (required): User ID
- **Request Body**: `UpdateUserRequestDto`
  ```json
  {
    "userType": "REGULAR"
  }
  ```
- **Response**: `UserResponseDto`
- **Status Codes**:
  - `200 OK`: User updated successfully
  - `400 Bad Request`: Invalid data
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.5 Delete User
- **Method**: `DELETE`
- **Path**: `/api/users/{userId}`
- **Description**: Delete a user by ID
- **Path Parameters**:
  - `userId` (required): User ID
- **Response**: No content
- **Status Codes**:
  - `204 No Content`: User deleted successfully
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

#### 2.6 Get Users by Type
- **Method**: `GET`
- **Path**: `/api/users/type/{userType}`
- **Description**: Retrieve users filtered by user type
- **Path Parameters**:
  - `userType` (required): User type (ADMIN or REGULAR)
- **Query Parameters**:
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
- **Response**: `Page<UserResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `400 Bad Request`: Invalid user type
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.7 Check if User is Admin
- **Method**: `GET`
- **Path**: `/api/users/{userId}/is-admin`
- **Description**: Check if a user has admin privileges
- **Path Parameters**:
  - `userId` (required): User ID
- **Response**: `Boolean`
- **Status Codes**:
  - `200 OK`: Check completed
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.8 Check if User Can View All Cards
- **Method**: `GET`
- **Path**: `/api/users/{userId}/can-view-all-cards`
- **Description**: Check if a user can view all credit cards without context
- **Path Parameters**:
  - `userId` (required): User ID
- **Response**: `Boolean`
- **Status Codes**:
  - `200 OK`: Check completed
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.9 Check if User Requires Account Context
- **Method**: `GET`
- **Path**: `/api/users/{userId}/requires-account-context`
- **Description**: Check if a user requires account context to view cards
- **Path Parameters**:
  - `userId` (required): User ID
- **Response**: `Boolean`
- **Status Codes**:
  - `200 OK`: Check completed
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

#### 2.10 Get Accessible Account IDs
- **Method**: `GET`
- **Path**: `/api/users/{userId}/accessible-accounts`
- **Description**: Get list of account IDs accessible by a user (admin users get all accounts, regular users get only their assigned accounts)
- **Path Parameters**:
  - `userId` (required): User ID
- **Response**: `List<String>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001

---

### 3. Credit Card Management API

**Base Path**: `/api/credit-cards`

#### 3.1 Get All Credit Cards
- **Method**: `GET`
- **Path**: `/api/credit-cards`
- **Description**: Retrieve a paginated list of all credit cards
- **Query Parameters**:
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort criteria
- **Response**: `Page<CreditCardResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `400 Bad Request`: Invalid parameters
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR014, BR015

#### 3.2 Get Credit Card by Card Number
- **Method**: `GET`
- **Path**: `/api/credit-cards/{cardNumber}`
- **Description**: Retrieve a specific credit card by its 16-digit card number
- **Path Parameters**:
  - `cardNumber` (required): 16-digit numeric card number
- **Response**: `CreditCardResponseDto`
- **Status Codes**:
  - `200 OK`: Card found
  - `400 Bad Request`: Invalid card number (must be 16 digits)
  - `404 Not Found`: Card not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR002

#### 3.3 Create Credit Card
- **Method**: `POST`
- **Path**: `/api/credit-cards`
- **Description**: Create a new credit card
- **Request Body**: `CreateCreditCardRequestDto`
  ```json
  {
    "cardNumber": "1234567890123456",
    "accountId": "12345678901",
    "cardStatus": "A"
  }
  ```
- **Response**: `CreditCardResponseDto`
- **Status Codes**:
  - `201 Created`: Card created successfully
  - `400 Bad Request`: Invalid data (validation errors)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR002, BR004, BR005

#### 3.4 Update Credit Card
- **Method**: `PUT`
- **Path**: `/api/credit-cards/{cardNumber}`
- **Description**: Update credit card details (primarily card status)
- **Path Parameters**:
  - `cardNumber` (required): 16-digit numeric card number
- **Request Body**: `UpdateCreditCardRequestDto`
  ```json
  {
    "cardStatus": "B"
  }
  ```
- **Response**: `CreditCardResponseDto`
- **Status Codes**:
  - `200 OK`: Card updated successfully
  - `400 Bad Request`: Invalid data
  - `404 Not Found`: Card not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR005, BR013

#### 3.5 Delete Credit Card
- **Method**: `DELETE`
- **Path**: `/api/credit-cards/{cardNumber}`
- **Description**: Delete a credit card by card number
- **Path Parameters**:
  - `cardNumber` (required): 16-digit numeric card number
- **Response**: No content
- **Status Codes**:
  - `204 No Content`: Card deleted successfully
  - `404 Not Found`: Card not found
  - `500 Internal Server Error`: Server error

#### 3.6 Filter Credit Cards
- **Method**: `POST`
- **Path**: `/api/credit-cards/filter`
- **Description**: Filter credit cards based on user permissions and filter criteria. Admin users can view all cards, regular users only see cards for their accessible accounts. All supplied filter criteria must match.
- **Query Parameters**:
  - `userId` (required): User ID requesting the cards
  - `page` (optional): Page number (0-based)
  - `size` (optional): Page size (default: 20)
- **Request Body**: `CreditCardFilterRequestDto`
  ```json
  {
    "cardNumber": "1234567890123456",
    "accountId": "12345678901",
    "cardStatus": "A",
    "page": 0,
    "size": 20
  }
  ```
- **Response**: `Page<CreditCardResponseDto>`
- **Status Codes**:
  - `200 OK`: Successful retrieval
  - `400 Bad Request`: Invalid filter parameters (card number must be 16 digits, account ID must be 11 digits)
  - `403 Forbidden`: Access denied (user has no accessible accounts)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001, BR002, BR004, BR005, BR006, BR014, BR015

#### 3.7 Validate Single Selection
- **Method**: `POST`
- **Path**: `/api/credit-cards/validate-selection`
- **Description**: Validate that only one credit card is selected for view or update operations
- **Query Parameters**:
  - `selectionCount` (required): Number of cards selected
- **Response**: No content
- **Status Codes**:
  - `200 OK`: Selection is valid
  - `400 Bad Request`: Invalid selection (only one card can be selected at a time)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR003

#### 3.8 Validate Backward Navigation
- **Method**: `POST`
- **Path**: `/api/credit-cards/validate-backward-navigation`
- **Description**: Validate navigation to previous page
- **Query Parameters**:
  - `page` (required): Current page number
- **Response**: No content
- **Status Codes**:
  - `200 OK`: Navigation is valid
  - `400 Bad Request`: Cannot navigate backward (NO PREVIOUS PAGES TO DISPLAY)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR008

#### 3.9 Validate Forward Navigation
- **Method**: `POST`
- **Path**: `/api/credit-cards/validate-forward-navigation`
- **Description**: Validate navigation to next page
- **Request Body**: Current page information
- **Response**: No content
- **Status Codes**:
  - `200 OK`: Navigation is valid
  - `400 Bad Request`: Cannot navigate forward (NO MORE PAGES TO DISPLAY)
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR009

#### 3.10 Check Row Selection Protection
- **Method**: `GET`
- **Path**: `/api/credit-cards/check-row-protection`
- **Description**: Check if row selection should be protected due to validation errors
- **Query Parameters**:
  - `hasValidationErrors` (required): Whether validation errors exist
- **Response**: `Boolean`
- **Status Codes**:
  - `200 OK`: Protection status returned
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR017

#### 3.11 Get View Details Target Program
- **Method**: `GET`
- **Path**: `/api/credit-cards/view-details-target`
- **Description**: Get the target program for viewing card details
- **Response**: `String` (returns "COCRDSLC")
- **Status Codes**:
  - `200 OK`: Target program returned
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR012

#### 3.12 Get Update Card Target Program
- **Method**: `GET`
- **Path**: `/api/credit-cards/update-card-target`
- **Description**: Get the target program for updating card information
- **Response**: `String` (returns "COCRDUPC")
- **Status Codes**:
  - `200 OK`: Target program returned
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR013

#### 3.13 Get Exit Menu Target Program
- **Method**: `GET`
- **Path**: `/api/credit-cards/exit-menu-target`
- **Description**: Get the target program for exiting to main menu
- **Response**: `String` (returns "COMEN01C")
- **Status Codes**:
  - `200 OK`: Target program returned
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR011

---

## Data Models

### AccountResponseDto
```json
{
  "accountId": "12345678901",
  "creditCardCount": 3,
  "userAccessCount": 2,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-20T14:45:00"
}
```

### UserResponseDto
```json
{
  "userId": "USER001",
  "userType": "ADMIN",
  "userTypeDisplayName": "Administrator",
  "isAdmin": true,
  "canViewAllCards": true,
  "requiresAccountContext": false,
  "accessibleAccountIds": ["12345678901", "23456789012"],
  "accessibleAccountCount": 2,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-20T14:45:00"
}
```

### CreditCardResponseDto
```json
{
  "cardNumber": "1234567890123456",
  "maskedCardNumber": "**** **** **** 3456",
  "formattedCardNumber": "1234 5678 9012 3456",
  "accountId": "12345678901",
  "cardStatus": "A",
  "cardStatusDisplayName": "Active",
  "isActive": true,
  "isBlocked": false,
  "isClosed": false,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-20T14:45:00"
}
```

---

## Card Status Codes

| Code | Display Name | Description |
|------|-------------|-------------|
| A | Active | Card is active and can be used |
| I | Inactive | Card is inactive |
| B | Blocked | Card is blocked |
| P | Pending | Card is pending activation |
| C | Closed | Card is closed |
| S | Suspended | Card is suspended |
| E | Expired | Card has expired |
| L | Lost | Card is reported as lost |
| T | Stolen | Card is reported as stolen |
| D | Damaged | Card is damaged |

---

## User Types

| Type | Code | Description | Permissions |
|------|------|-------------|-------------|
| Administrator | ADMIN | Admin user | Can view all credit cards without context, full access to all accounts |
| Regular User | REGULAR | Regular user | Can only view cards associated with their specific accounts, requires account context |

---

## Error Codes and Messages

### Validation Errors

| Error Code | Message | Business Rule |
|------------|---------|---------------|
| CARD_FILTER_INVALID | CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER | BR002 |
| CARD_FILTER_BLANK | CARD FILTER CANNOT BE BLANK, SPACES OR ZEROS | BR002 |
| ACCOUNT_FILTER_INVALID | ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER | BR004 |
| ACCOUNT_FILTER_BLANK | ACCOUNT FILTER CANNOT BE BLANK, SPACES OR ZEROS | BR004 |
| MULTIPLE_SELECTION | Only one credit card can be selected at a time for view or update operations | BR003 |
| NO_PREVIOUS_PAGES | NO PREVIOUS PAGES TO DISPLAY | BR008 |
| NO_MORE_PAGES | NO MORE PAGES TO DISPLAY | BR009 |

---

## Database Schema

### Tables

1. **accounts**
   - `account_id` VARCHAR(11) PRIMARY KEY
   - `created_at` TIMESTAMP
   - `updated_at` TIMESTAMP

2. **users**
   - `user_id` VARCHAR(20) PRIMARY KEY
   - `user_type` VARCHAR(10) (ADMIN or REGULAR)
   - `created_at` TIMESTAMP
   - `updated_at` TIMESTAMP

3. **credit_cards**
   - `card_number` VARCHAR(16) PRIMARY KEY
   - `account_id` VARCHAR(11) FOREIGN KEY → accounts(account_id)
   - `card_status` CHAR(1)
   - `created_at` TIMESTAMP
   - `updated_at` TIMESTAMP

4. **user_account_access**
   - `user_id` VARCHAR(20) FOREIGN KEY → users(user_id)
   - `account_id` VARCHAR(11) FOREIGN KEY → accounts(account_id)
   - `granted_at` TIMESTAMP
   - PRIMARY KEY (user_id, account_id)

---

## Sample Data

The system includes sample data for testing:

- **5 Accounts**: 12345678901, 23456789012, 34567890123, 45678901234, 56789012345
- **4 Users**: 
  - ADMIN001 (Admin)
  - USER001, USER002, USER003 (Regular users)
- **10 Credit Cards**: Various cards with different statuses across all accounts
- **User Access Mappings**: Regular users have access to specific accounts

---

## Authentication and Authorization

**Note**: The current implementation does not include authentication/authorization middleware. In a production environment, you should:

1. Add Spring Security for authentication
2. Implement JWT or OAuth2 for token-based authentication
3. Add role-based access control (RBAC)
4. Secure all endpoints with appropriate permissions
5. Implement user session management

---

## Pagination

All list endpoints support pagination with the following parameters:

- `page`: Page number (0-based, default: 0)
- `size`: Page size (default: 20)
- `sort`: Sort criteria (e.g., "createdAt,desc")

Response includes:
- `content`: Array of items
- `totalElements`: Total number of items
- `totalPages`: Total number of pages
- `size`: Page size
- `number`: Current page number
- `first`: Is first page
- `last`: Is last page

---

## Testing the API

### Using cURL

```bash
# Get all accounts
curl -X GET http://localhost:8080/api/accounts

# Create a new account
curl -X POST http://localhost:8080/api/accounts \
  -H "Content-Type: application/json" \
  -d '{"accountId":"98765432109"}'

# Filter credit cards for a user
curl -X POST http://localhost:8080/api/credit-cards/filter?userId=USER001 \
  -H "Content-Type: application/json" \
  -d '{"accountId":"12345678901","cardStatus":"A"}'

# Check if user is admin
curl -X GET http://localhost:8080/api/users/ADMIN001/is-admin
```

### Using Swagger UI

Access the Swagger UI at: `http://localhost:8080/swagger-ui.html`

---

## Deployment Notes

1. **Database Setup**: Ensure PostgreSQL is running and configured in `application.properties`
2. **Flyway Migrations**: Migrations will run automatically on application startup
3. **Sample Data**: Sample data is included in V5 migration for testing
4. **Port Configuration**: Default port is 8080, configurable in `application.properties`

---

## Support and Maintenance

For issues, questions, or feature requests, please refer to the project documentation or contact the development team.

**Generated**: 2024
**Version**: 1.0.0
**Framework**: Spring Boot 3.5.5
**Java Version**: 21
