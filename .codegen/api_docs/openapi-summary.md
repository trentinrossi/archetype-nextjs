# API Documentation Summary

## Card and Account Management System

This document provides a comprehensive overview of all REST API endpoints for the Card and Account Management system.

---

## Table of Contents

1. [Account Management APIs](#account-management-apis)
2. [User Management APIs](#user-management-apis)
3. [Credit Card Management APIs](#credit-card-management-apis)
4. [Business Rules Implementation](#business-rules-implementation)
5. [Error Codes and Responses](#error-codes-and-responses)
6. [Data Validation Rules](#data-validation-rules)

---

## Account Management APIs

Base Path: `/api/accounts`

### 1. Get All Accounts
- **Endpoint**: `GET /api/accounts`
- **Description**: Retrieve a paginated list of all accounts
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort field and direction
- **Response**: `200 OK`
  ```json
  {
    "content": [
      {
        "accountId": "12345678901",
        "creditCardCount": 3,
        "createdAt": "2024-01-15T10:30:00",
        "updatedAt": "2024-01-20T14:45:00"
      }
    ],
    "pageable": {...},
    "totalElements": 100,
    "totalPages": 5
  }
  ```
- **Error Responses**:
  - `400 Bad Request`: Invalid pagination parameters
  - `500 Internal Server Error`: Server error

### 2. Get Account by ID
- **Endpoint**: `GET /api/accounts/{accountId}`
- **Description**: Retrieve an account by its 11-digit identifier
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Response**: `200 OK`
  ```json
  {
    "accountId": "12345678901",
    "creditCardCount": 3,
    "createdAt": "2024-01-15T10:30:00",
    "updatedAt": "2024-01-20T14:45:00"
  }
  ```
- **Error Responses**:
  - `404 Not Found`: Account not found
  - `500 Internal Server Error`: Server error

### 3. Create Account
- **Endpoint**: `POST /api/accounts`
- **Description**: Create a new customer account with 11-digit ID
- **Request Body**:
  ```json
  {
    "accountId": "12345678901"
  }
  ```
- **Validation Rules**:
  - `accountId`: Required, must be exactly 11 digits
- **Response**: `201 Created`
  ```json
  {
    "accountId": "12345678901",
    "creditCardCount": 0,
    "createdAt": "2024-01-15T10:30:00",
    "updatedAt": "2024-01-15T10:30:00"
  }
  ```
- **Error Responses**:
  - `400 Bad Request`: Invalid account ID format or account already exists
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR003 - Account ID must be valid 11-digit numeric value

### 4. Delete Account
- **Endpoint**: `DELETE /api/accounts/{accountId}`
- **Description**: Delete an account by its ID
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Response**: `204 No Content`
- **Error Responses**:
  - `404 Not Found`: Account not found
  - `500 Internal Server Error`: Server error

### 5. Check Account Existence
- **Endpoint**: `GET /api/accounts/{accountId}/exists`
- **Description**: Validate if an account exists by its ID
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Response**: `200 OK`
  ```json
  true
  ```
- **Error Responses**:
  - `500 Internal Server Error`: Server error

---

## User Management APIs

Base Path: `/api/users`

### 1. Get All Users
- **Endpoint**: `GET /api/users`
- **Description**: Retrieve a paginated list of all users
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort field and direction
- **Response**: `200 OK`
  ```json
  {
    "content": [
      {
        "id": 1,
        "username": "john.doe",
        "userType": "REGULAR",
        "userTypeDisplayName": "Regular User",
        "accountId": "12345678901",
        "email": "john.doe@example.com",
        "firstName": "John",
        "lastName": "Doe",
        "fullName": "John Doe",
        "active": true,
        "isAdmin": false,
        "canViewAllCards": false,
        "createdAt": "2024-01-15T10:30:00",
        "updatedAt": "2024-01-20T14:45:00"
      }
    ],
    "pageable": {...},
    "totalElements": 50,
    "totalPages": 3
  }
  ```
- **Error Responses**:
  - `400 Bad Request`: Invalid pagination parameters
  - `500 Internal Server Error`: Server error

### 2. Get User by ID
- **Endpoint**: `GET /api/users/{id}`
- **Description**: Retrieve a user by their ID
- **Path Parameters**:
  - `id` (required): User identifier
- **Response**: `200 OK` (same structure as above)
- **Error Responses**:
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

### 3. Get User by Username
- **Endpoint**: `GET /api/users/username/{username}`
- **Description**: Retrieve a user by their username
- **Path Parameters**:
  - `username` (required): Username
- **Response**: `200 OK` (same structure as Get User by ID)
- **Error Responses**:
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

### 4. Get Users by Type
- **Endpoint**: `GET /api/users/type/{userType}`
- **Description**: Retrieve users filtered by type (ADMIN or REGULAR)
- **Path Parameters**:
  - `userType` (required): User type (ADMIN or REGULAR)
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid user type
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001 - User type determines permissions

### 5. Search Users
- **Endpoint**: `GET /api/users/search`
- **Description**: Search users by name or username
- **Query Parameters**:
  - `searchTerm` (required): Search term
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid search parameters
  - `500 Internal Server Error`: Server error

### 6. Create User
- **Endpoint**: `POST /api/users`
- **Description**: Create a new system user with specified permissions
- **Request Body**:
  ```json
  {
    "username": "john.doe",
    "userType": "REGULAR",
    "accountId": "12345678901",
    "email": "john.doe@example.com",
    "firstName": "John",
    "lastName": "Doe"
  }
  ```
- **Validation Rules**:
  - `username`: Required
  - `userType`: Required (ADMIN or REGULAR)
  - `accountId`: Required for REGULAR users, must be 11 digits
  - `email`: Required, must be valid email format
- **Response**: `201 Created`
- **Error Responses**:
  - `400 Bad Request`: Invalid data, username/email exists, or validation failure
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR001 - User type determines permissions

### 7. Update User
- **Endpoint**: `PUT /api/users/{id}`
- **Description**: Update user details by ID
- **Path Parameters**:
  - `id` (required): User identifier
- **Request Body**:
  ```json
  {
    "userType": "REGULAR",
    "accountId": "12345678901",
    "email": "john.doe@example.com",
    "firstName": "John",
    "lastName": "Doe",
    "active": true
  }
  ```
- **Validation Rules**:
  - `accountId`: Must be 11 digits if provided
  - `email`: Must be valid email format if provided
- **Response**: `200 OK`
- **Error Responses**:
  - `400 Bad Request`: Invalid data or validation failure
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

### 8. Delete User
- **Endpoint**: `DELETE /api/users/{id}`
- **Description**: Delete a user by ID
- **Path Parameters**:
  - `id` (required): User identifier
- **Response**: `204 No Content`
- **Error Responses**:
  - `404 Not Found`: User not found
  - `500 Internal Server Error`: Server error

---

## Credit Card Management APIs

Base Path: `/api/credit-cards`

### 1. Get All Credit Cards
- **Endpoint**: `GET /api/credit-cards`
- **Description**: Retrieve a paginated list of all credit cards (Admin access)
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
  - `sort` (optional): Sort field and direction
- **Response**: `200 OK`
  ```json
  {
    "content": [
      {
        "cardNumber": "1234567890123456",
        "maskedCardNumber": "************3456",
        "accountId": "12345678901",
        "cardStatus": "A",
        "cardStatusEnum": "ACTIVE",
        "cardStatusDisplayName": "Active",
        "cardholderName": "John Doe",
        "expiryMonth": "12",
        "expiryYear": "2025",
        "cardType": "VISA",
        "creditLimit": 5000.00,
        "availableCredit": 4500.00,
        "isActive": true,
        "isExpired": false,
        "canModify": true,
        "createdAt": "2024-01-15T10:30:00",
        "updatedAt": "2024-01-20T14:45:00"
      }
    ],
    "pageable": {...},
    "totalElements": 200,
    "totalPages": 10
  }
  ```
- **Error Responses**:
  - `400 Bad Request`: Invalid pagination parameters
  - `500 Internal Server Error`: Server error
- **Business Rules**: 
  - BR001 - Admin users can view all cards when no context is passed
  - BR010 - When no records match, appropriate message is displayed

### 2. Get Credit Card by Number
- **Endpoint**: `GET /api/credit-cards/{cardNumber}`
- **Description**: Retrieve a credit card by its 16-digit number
- **Path Parameters**:
  - `cardNumber` (required): 16-digit card number
- **Response**: `200 OK` (same structure as above)
- **Error Responses**:
  - `404 Not Found`: Credit card not found
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR005 - Single row selection enforcement

### 3. Get Credit Cards by Account
- **Endpoint**: `GET /api/credit-cards/account/{accountId}`
- **Description**: Retrieve credit cards for a specific account (11-digit ID)
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid account ID format
  - `500 Internal Server Error`: Server error
- **Business Rules**: 
  - BR001 - Non-admin users can only view cards associated with their specific account
  - BR003 - Account ID must be valid 11-digit numeric value
  - BR007 - Records are filtered based on account ID criteria

### 4. Get Credit Cards by Status
- **Endpoint**: `GET /api/credit-cards/status/{cardStatus}`
- **Description**: Retrieve credit cards filtered by status code
- **Path Parameters**:
  - `cardStatus` (required): Status code (A, I, B, C, S)
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid status code
  - `500 Internal Server Error`: Server error

### 5. Get Credit Cards by Account and Status
- **Endpoint**: `GET /api/credit-cards/account/{accountId}/status/{cardStatus}`
- **Description**: Retrieve credit cards filtered by both account ID and status
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
  - `cardStatus` (required): Status code
- **Query Parameters**:
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid parameters
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR007 - Both filters can be applied simultaneously

### 6. Search Credit Cards by Number
- **Endpoint**: `GET /api/credit-cards/search`
- **Description**: Search credit cards by card number pattern
- **Query Parameters**:
  - `cardNumberPattern` (required): Card number pattern to search
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid search parameters
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR007 - Records are filtered based on card number criteria

### 7. Search Credit Cards by Account and Number
- **Endpoint**: `GET /api/credit-cards/account/{accountId}/search`
- **Description**: Search credit cards by account ID and card number pattern
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Query Parameters**:
  - `cardNumberPattern` (required): Card number pattern
  - `page` (optional): Page number (default: 0)
  - `size` (optional): Page size (default: 20)
- **Response**: `200 OK` (paginated list)
- **Error Responses**:
  - `400 Bad Request`: Invalid parameters
  - `500 Internal Server Error`: Server error
- **Business Rules**: BR007 - Both filters can be applied simultaneously

### 8. Create Credit Card
- **Endpoint**: `POST /api/credit-cards`
- **Description**: Create a new credit card with 16-digit number and 11-digit account ID
- **Request Body**:
  ```json
  {
    "cardNumber": "1234567890123456",
    "accountId": "12345678901",
    "cardStatus": "A",
    "cardholderName": "John Doe",
    "expiryMonth": "12",
    "expiryYear": "2025",
    "cardType": "VISA",
    "creditLimit": 5000.00,
    "availableCredit": 5000.00
  }
  ```
- **Validation Rules**:
  - `cardNumber`: Required, must be exactly 16 digits
  - `accountId`: Required, must be exactly 11 digits
  - `cardStatus`: Required, must be single uppercase letter
  - `expiryMonth`: Optional, must be 01-12 if provided
  - `expiryYear`: Optional, must be 4 digits if provided
- **Response**: `201 Created`
- **Error Responses**:
  - `400 Bad Request`: Invalid data, card exists, or validation failure
  - `500 Internal Server Error`: Server error

### 9. Update Credit Card
- **Endpoint**: `PUT /api/credit-cards/{cardNumber}`
- **Description**: Update credit card details by card number
- **Path Parameters**:
  - `cardNumber` (required): 16-digit card number
- **Request Body**:
  ```json
  {
    "cardStatus": "A",
    "cardholderName": "John Doe",
    "expiryMonth": "12",
    "expiryYear": "2025",
    "cardType": "VISA",
    "creditLimit": 5000.00,
    "availableCredit": 4500.00
  }
  ```
- **Validation Rules**:
  - `cardStatus`: Must be single uppercase letter if provided
  - `expiryMonth`: Must be 01-12 if provided
  - `expiryYear`: Must be 4 digits if provided
- **Response**: `200 OK`
- **Error Responses**:
  - `400 Bad Request`: Invalid data or card cannot be modified (cancelled)
  - `404 Not Found`: Credit card not found
  - `500 Internal Server Error`: Server error

### 10. Delete Credit Card
- **Endpoint**: `DELETE /api/credit-cards/{cardNumber}`
- **Description**: Delete a credit card by card number
- **Path Parameters**:
  - `cardNumber` (required): 16-digit card number
- **Response**: `204 No Content`
- **Error Responses**:
  - `404 Not Found`: Credit card not found
  - `500 Internal Server Error`: Server error

### 11. Count Credit Cards by Account
- **Endpoint**: `GET /api/credit-cards/account/{accountId}/count`
- **Description**: Get the count of credit cards for a specific account
- **Path Parameters**:
  - `accountId` (required): 11-digit account identifier
- **Response**: `200 OK`
  ```json
  3
  ```
- **Error Responses**:
  - `500 Internal Server Error`: Server error

---

## Business Rules Implementation

### BR001: User Permission Based Card Listing
- **Implementation**: User type determines access permissions
- **ADMIN Users**: Can view all cards when no context is passed (GET /api/credit-cards)
- **REGULAR Users**: Can only view cards associated with their specific account (GET /api/credit-cards/account/{accountId})
- **Endpoints Affected**: All credit card listing endpoints

### BR003: Account ID Filter Validation
- **Implementation**: Account ID must be valid 11-digit numeric value
- **Validation**: Pattern matching `^\\d{11}$`
- **Error Handling**: Returns 400 Bad Request for invalid format
- **Endpoints Affected**: All endpoints accepting accountId parameter

### BR005: Single Row Selection Enforcement
- **Implementation**: Users can only select one credit card at a time
- **Endpoints**: GET /api/credit-cards/{cardNumber}
- **Behavior**: Returns single card details, not a list

### BR007: Record Filtering Logic
- **Implementation**: Records are filtered based on account ID and/or card number criteria
- **Supported Filters**:
  - Account ID only
  - Card status only
  - Account ID + Card status
  - Card number pattern
  - Account ID + Card number pattern
- **Endpoints**: Multiple search and filter endpoints

### BR008: First Page Navigation Restriction
- **Implementation**: Handled by Spring Data pagination
- **Behavior**: When on first page (page=0), previous page navigation returns empty

### BR009: Last Page Navigation Restriction
- **Implementation**: Handled by Spring Data pagination
- **Behavior**: When on last page, next page navigation returns empty

### BR010: No Records Found Handling
- **Implementation**: Returns empty page with appropriate metadata
- **Response**: 200 OK with empty content array
- **Endpoints**: All paginated endpoints

### BR011: Program Initialization on First Entry
- **Implementation**: Default values set in entity constructors
- **Behavior**: Working storage initialized with defaults

### BR012: Context Preservation Between Interactions
- **Implementation**: Stateless REST API with pagination support
- **Behavior**: State maintained through query parameters (page, size, filters)

---

## Error Codes and Responses

### HTTP Status Codes

| Code | Description | When Used |
|------|-------------|-----------|
| 200 | OK | Successful GET request |
| 201 | Created | Successful POST request |
| 204 | No Content | Successful DELETE request |
| 400 | Bad Request | Invalid input data or validation failure |
| 404 | Not Found | Resource not found |
| 500 | Internal Server Error | Server-side error |

### Common Error Response Format

```json
{
  "timestamp": "2024-01-20T14:45:00",
  "status": 400,
  "error": "Bad Request",
  "message": "Account ID must be exactly 11 digits",
  "path": "/api/accounts"
}
```

### Validation Error Messages

| Field | Error Message |
|-------|---------------|
| accountId | "Account ID must be exactly 11 digits" |
| cardNumber | "Card number must be exactly 16 digits" |
| cardStatus | "Card status must be a single uppercase letter" |
| email | "Email must be valid" |
| expiryMonth | "Expiry month must be between 01 and 12" |
| expiryYear | "Expiry year must be 4 digits" |

---

## Data Validation Rules

### Account
- **accountId**: Required, exactly 11 digits, numeric only

### User
- **username**: Required, max 50 characters, unique
- **userType**: Required, must be ADMIN or REGULAR
- **accountId**: Required for REGULAR users, exactly 11 digits
- **email**: Required, valid email format, unique, max 255 characters
- **firstName**: Optional, max 100 characters
- **lastName**: Optional, max 100 characters

### Credit Card
- **cardNumber**: Required, exactly 16 digits, numeric only, unique
- **accountId**: Required, exactly 11 digits, must exist in accounts table
- **cardStatus**: Required, single uppercase letter (A, I, B, C, S)
- **cardholderName**: Optional, max 100 characters
- **expiryMonth**: Optional, 2 digits (01-12)
- **expiryYear**: Optional, 4 digits
- **cardType**: Optional, max 20 characters
- **creditLimit**: Optional, must be >= 0
- **availableCredit**: Optional, must be >= 0

---

## Card Status Codes

| Code | Status | Description | Can Modify | Can Use |
|------|--------|-------------|------------|---------|
| A | Active | Card is active and usable | Yes | Yes |
| I | Inactive | Card is inactive | Yes | No |
| B | Blocked | Card is blocked | Yes | No |
| C | Cancelled | Card is cancelled | No | No |
| S | Suspended | Card is suspended | Yes | No |

---

## Pagination

All list endpoints support pagination with the following parameters:

- **page**: Page number (0-indexed, default: 0)
- **size**: Number of items per page (default: 20)
- **sort**: Sort field and direction (e.g., "createdAt,desc")

### Pagination Response Structure

```json
{
  "content": [...],
  "pageable": {
    "sort": {...},
    "offset": 0,
    "pageNumber": 0,
    "pageSize": 20,
    "paged": true,
    "unpaged": false
  },
  "last": false,
  "totalPages": 10,
  "totalElements": 200,
  "size": 20,
  "number": 0,
  "sort": {...},
  "first": true,
  "numberOfElements": 20,
  "empty": false
}
```

---

## Sample Usage Examples

### Example 1: Admin User Views All Cards

```bash
GET /api/credit-cards?page=0&size=20
```

### Example 2: Regular User Views Their Cards

```bash
GET /api/credit-cards/account/12345678901?page=0&size=20
```

### Example 3: Filter Cards by Account and Status

```bash
GET /api/credit-cards/account/12345678901/status/A?page=0&size=20
```

### Example 4: Search Cards by Number Pattern

```bash
GET /api/credit-cards/search?cardNumberPattern=1234&page=0&size=20
```

### Example 5: Create New Credit Card

```bash
POST /api/credit-cards
Content-Type: application/json

{
  "cardNumber": "1234567890123456",
  "accountId": "12345678901",
  "cardStatus": "A",
  "cardholderName": "John Doe",
  "expiryMonth": "12",
  "expiryYear": "2025",
  "cardType": "VISA",
  "creditLimit": 5000.00,
  "availableCredit": 5000.00
}
```

---

## Notes

1. All timestamps are in ISO 8601 format with timezone
2. All monetary values are in decimal format with 2 decimal places
3. Card numbers are masked in responses for security (shows last 4 digits)
4. Pagination is 0-indexed (first page is page=0)
5. All endpoints support sorting by any field using the `sort` parameter
6. Authentication and authorization should be implemented based on user type
7. Regular users should only have access to their account's data
8. Admin users have unrestricted access to all data

---

## Technology Stack

- **Framework**: Spring Boot 3.5.5
- **Java Version**: 21
- **Database**: PostgreSQL
- **ORM**: Spring Data JPA
- **Migration**: Flyway
- **Documentation**: OpenAPI/Swagger
- **Validation**: Jakarta Bean Validation

---

## Getting Started

1. Ensure PostgreSQL is running
2. Configure database connection in `application.properties`
3. Run the application: `mvn spring-boot:run`
4. Access Swagger UI: `http://localhost:8080/swagger-ui.html`
5. Test endpoints using the interactive documentation

---

*Generated: 2024-01-20*
*Version: 1.0.0*
*Application: Card and Account Management System*
