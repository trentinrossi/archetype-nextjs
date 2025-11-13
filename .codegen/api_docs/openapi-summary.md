# CardDemo Login and User Management API - OpenAPI Summary

## Overview

This document provides a comprehensive overview of the REST API endpoints for the CardDemo Login and User Management system. The API is built using Spring Boot 3.5.5 with Java 21 and follows RESTful principles.

**Base URL:** `http://localhost:8080/api`

**API Version:** 1.0.0

---

## Table of Contents

1. [User Management API](#user-management-api)
2. [User Session Management API](#user-session-management-api)
3. [Menu Option Management API](#menu-option-management-api)
4. [Admin Menu Option Management API](#admin-menu-option-management-api)
5. [Data Models](#data-models)
6. [Error Codes](#error-codes)
7. [Business Rules Implementation](#business-rules-implementation)

---

## User Management API

### Tag: User Management
**Description:** APIs for managing users in CardDemo Admin Menu system

### Endpoints

#### 1. Get All Users
- **Method:** `GET`
- **Path:** `/api/users`
- **Description:** Retrieve a paginated list of all users in the CardDemo system
- **Parameters:**
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
  - `sort` (query, optional): Sort criteria
- **Response Codes:**
  - `200 OK`: Successful retrieval of users
  - `400 Bad Request`: Invalid request parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserResponseDto>`

#### 2. Get User by ID
- **Method:** `GET`
- **Path:** `/api/users/{id}`
- **Description:** Retrieve a specific user by their database ID
- **Parameters:**
  - `id` (path, required): User database ID
- **Response Codes:**
  - `200 OK`: Successful retrieval of user
  - `404 Not Found`: User ID NOT found...
  - `500 Internal Server Error`: Unable to lookup User...
- **Response Body:** `UserResponseDto`

#### 3. Get User by User ID
- **Method:** `GET`
- **Path:** `/api/users/by-user-id/{userId}`
- **Description:** Retrieve a specific user by their user ID (8 character identifier)
- **Parameters:**
  - `userId` (path, required): User ID (8 characters)
- **Response Codes:**
  - `200 OK`: Successful retrieval of user
  - `404 Not Found`: User ID NOT found...
  - `500 Internal Server Error`: Unable to lookup User...
- **Response Body:** `UserResponseDto`

#### 4. Create User
- **Method:** `POST`
- **Path:** `/api/users`
- **Description:** Create a new user in the CardDemo system with authentication credentials
- **Request Body:** `CreateUserRequestDto`
- **Response Codes:**
  - `201 Created`: User created successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `409 Conflict`: User ID already exist...
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserResponseDto`

#### 5. Update User
- **Method:** `PUT`
- **Path:** `/api/users/{id}`
- **Description:** Update user details by database ID
- **Parameters:**
  - `id` (path, required): User database ID
- **Request Body:** `UpdateUserRequestDto`
- **Response Codes:**
  - `200 OK`: User updated successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `404 Not Found`: User ID NOT found...
  - `500 Internal Server Error`: Unable to Update User...
- **Response Body:** `UserResponseDto`

#### 6. Delete User
- **Method:** `DELETE`
- **Path:** `/api/users/{id}`
- **Description:** Delete a user by their database ID
- **Parameters:**
  - `id` (path, required): User database ID
- **Response Codes:**
  - `204 No Content`: User deleted successfully
  - `404 Not Found`: User ID NOT found...
  - `500 Internal Server Error`: Unable to Update User...

#### 7. User Authentication (Login)
- **Method:** `POST`
- **Path:** `/api/users/login`
- **Description:** Authenticate user with user ID and password for admin menu access (BR001 - User Authentication Check)
- **Request Body:** `LoginRequestDto`
- **Response Codes:**
  - `200 OK`: Authentication successful
  - `400 Bad Request`: Please enter User ID ... / Please enter Password ...
  - `401 Unauthorized`: Wrong Password. Try again ... / User not found. Try again ...
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `LoginResponseDto`

#### 8. Check Admin Authentication
- **Method:** `GET`
- **Path:** `/api/users/by-user-id/{userId}/admin-check`
- **Description:** Verify that user is authenticated as admin before accessing admin menu (BR001 - Admin Authentication Requirement)
- **Parameters:**
  - `userId` (path, required): User ID (8 characters)
- **Response Codes:**
  - `200 OK`: User is authenticated as admin
  - `401 Unauthorized`: User is not authenticated or not an admin
  - `500 Internal Server Error`: Internal server error

#### 9. Search Users
- **Method:** `GET`
- **Path:** `/api/users/search`
- **Description:** Search users by name (first name or last name)
- **Parameters:**
  - `searchTerm` (query, required): Search term for name search
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful search results
  - `400 Bad Request`: Invalid search parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserResponseDto>`

#### 10. Filter Users by Type
- **Method:** `GET`
- **Path:** `/api/users/filter-by-type`
- **Description:** Filter users by user type (admin or regular)
- **Parameters:**
  - `userType` (query, required): User type (A=Admin, R=Regular)
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful retrieval of filtered users
  - `400 Bad Request`: User Type can NOT be empty...
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserResponseDto>`

---

## User Session Management API

### Tag: User Session Management
**Description:** APIs for managing user sessions and program navigation

### Endpoints

#### 1. Get All Sessions
- **Method:** `GET`
- **Path:** `/api/sessions`
- **Description:** Retrieve a paginated list of all user sessions
- **Parameters:**
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful retrieval of sessions
  - `400 Bad Request`: Invalid request parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserSessionResponseDto>`

#### 2. Get Session by ID
- **Method:** `GET`
- **Path:** `/api/sessions/{id}`
- **Description:** Retrieve a specific session by its unique identifier
- **Parameters:**
  - `id` (path, required): Session ID
- **Response Codes:**
  - `200 OK`: Successful retrieval of session
  - `404 Not Found`: Session not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserSessionResponseDto`

#### 3. Get Latest Session by User ID
- **Method:** `GET`
- **Path:** `/api/sessions/user/{userId}/latest`
- **Description:** Retrieve the most recent session for a specific user
- **Parameters:**
  - `userId` (path, required): User ID (8 characters)
- **Response Codes:**
  - `200 OK`: Successful retrieval of session
  - `404 Not Found`: Session not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserSessionResponseDto`

#### 4. Create Session
- **Method:** `POST`
- **Path:** `/api/sessions`
- **Description:** Create a new user session for program navigation and context tracking
- **Request Body:** `CreateUserSessionRequestDto`
- **Response Codes:**
  - `201 Created`: Session created successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserSessionResponseDto`

#### 5. Update Session
- **Method:** `PUT`
- **Path:** `/api/sessions/{id}`
- **Description:** Update session details by session ID
- **Parameters:**
  - `id` (path, required): Session ID
- **Request Body:** `UpdateUserSessionRequestDto`
- **Response Codes:**
  - `200 OK`: Session updated successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `404 Not Found`: Session not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserSessionResponseDto`

#### 6. Delete Session
- **Method:** `DELETE`
- **Path:** `/api/sessions/{id}`
- **Description:** Delete a session by its ID
- **Parameters:**
  - `id` (path, required): Session ID
- **Response Codes:**
  - `204 No Content`: Session deleted successfully
  - `404 Not Found`: Session not found
  - `500 Internal Server Error`: Internal server error

#### 7. Get Sessions by User ID
- **Method:** `GET`
- **Path:** `/api/sessions/user/{userId}`
- **Description:** Retrieve all sessions for a specific user
- **Parameters:**
  - `userId` (path, required): User ID (8 characters)
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful retrieval of sessions
  - `400 Bad Request`: Invalid request parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserSessionResponseDto>`

#### 8. Get Sessions by User Type
- **Method:** `GET`
- **Path:** `/api/sessions/user-type/{userType}`
- **Description:** Retrieve all sessions for a specific user type (admin or regular)
- **Parameters:**
  - `userType` (path, required): User type (A=Admin, R=Regular)
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful retrieval of sessions
  - `400 Bad Request`: Invalid request parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<UserSessionResponseDto>`

#### 9. Transfer Session to Program
- **Method:** `POST`
- **Path:** `/api/sessions/{id}/transfer`
- **Description:** Transfer a session to a different program (BR004 - Program Navigation)
- **Parameters:**
  - `id` (path, required): Session ID
  - `targetProgram` (query, required): Target program name
  - `targetTransaction` (query, required): Target transaction ID
- **Response Codes:**
  - `200 OK`: Session transferred successfully
  - `404 Not Found`: Session not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `UserSessionResponseDto`

---

## Menu Option Management API

### Tag: Menu Option Management
**Description:** APIs for managing menu options and navigation

### Endpoints

#### 1. Get All Menu Options
- **Method:** `GET`
- **Path:** `/api/menu-options`
- **Description:** Retrieve a paginated list of all menu options
- **Parameters:**
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful retrieval of menu options
  - `400 Bad Request`: Invalid request parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<MenuOptionResponseDto>`

#### 2. Get Menu Option by ID
- **Method:** `GET`
- **Path:** `/api/menu-options/{id}`
- **Description:** Retrieve a specific menu option by its unique identifier
- **Parameters:**
  - `id` (path, required): Menu option ID
- **Response Codes:**
  - `200 OK`: Successful retrieval of menu option
  - `404 Not Found`: Menu option not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `MenuOptionResponseDto`

#### 3. Get Menu Option by Number
- **Method:** `GET`
- **Path:** `/api/menu-options/by-number/{optionNumber}`
- **Description:** Retrieve a specific menu option by its option number
- **Parameters:**
  - `optionNumber` (path, required): Menu option number
- **Response Codes:**
  - `200 OK`: Successful retrieval of menu option
  - `404 Not Found`: Menu option not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `MenuOptionResponseDto`

#### 4. Create Menu Option
- **Method:** `POST`
- **Path:** `/api/menu-options`
- **Description:** Create a new menu option for the system
- **Request Body:** `CreateMenuOptionRequestDto`
- **Response Codes:**
  - `201 Created`: Menu option created successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `409 Conflict`: Menu option number already exists
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `MenuOptionResponseDto`

#### 5. Update Menu Option
- **Method:** `PUT`
- **Path:** `/api/menu-options/{id}`
- **Description:** Update menu option details by ID
- **Parameters:**
  - `id` (path, required): Menu option ID
- **Request Body:** `UpdateMenuOptionRequestDto`
- **Response Codes:**
  - `200 OK`: Menu option updated successfully
  - `400 Bad Request`: Invalid request data - validation errors
  - `404 Not Found`: Menu option not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `MenuOptionResponseDto`

#### 6. Delete Menu Option
- **Method:** `DELETE`
- **Path:** `/api/menu-options/{id}`
- **Description:** Delete a menu option by its ID
- **Parameters:**
  - `id` (path, required): Menu option ID
- **Response Codes:**
  - `204 No Content`: Menu option deleted successfully
  - `404 Not Found`: Menu option not found
  - `500 Internal Server Error`: Internal server error

#### 7. Get Active Menu Options
- **Method:** `GET`
- **Path:** `/api/menu-options/active`
- **Description:** Retrieve all active menu options ordered by display order
- **Response Codes:**
  - `200 OK`: Successful retrieval of active menu options
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `List<MenuOptionResponseDto>`

#### 8. Get Menu Options for User Type
- **Method:** `GET`
- **Path:** `/api/menu-options/for-user-type/{userType}`
- **Description:** Retrieve menu options accessible by a specific user type (BR003 - Access Control by User Type)
- **Parameters:**
  - `userType` (path, required): User type (A=Admin, R=Regular)
- **Response Codes:**
  - `200 OK`: Successful retrieval of menu options
  - `400 Bad Request`: Invalid user type
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `List<MenuOptionResponseDto>`

#### 9. Get Coming Soon Options
- **Method:** `GET`
- **Path:** `/api/menu-options/coming-soon`
- **Description:** Retrieve menu options that are marked as coming soon (BR005 - Coming Soon Feature Handling)
- **Response Codes:**
  - `200 OK`: Successful retrieval of coming soon options
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `List<MenuOptionResponseDto>`

#### 10. Get Implemented Options
- **Method:** `GET`
- **Path:** `/api/menu-options/implemented`
- **Description:** Retrieve menu options that are fully implemented and active
- **Response Codes:**
  - `200 OK`: Successful retrieval of implemented options
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `List<MenuOptionResponseDto>`

#### 11. Search Menu Options
- **Method:** `GET`
- **Path:** `/api/menu-options/search`
- **Description:** Search menu options by name
- **Parameters:**
  - `searchTerm` (query, required): Search term for option name
  - `page` (query, optional): Page number (default: 0)
  - `size` (query, optional): Page size (default: 20)
- **Response Codes:**
  - `200 OK`: Successful search results
  - `400 Bad Request`: Invalid search parameters
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Page<MenuOptionResponseDto>`

#### 12. Validate Menu Option
- **Method:** `GET`
- **Path:** `/api/menu-options/validate/{optionNumber}`
- **Description:** Validate if a menu option number is valid (BR002 - Menu Option Validation)
- **Parameters:**
  - `optionNumber` (path, required): Menu option number to validate
  - `maxOptions` (query, required): Maximum option number allowed
- **Response Codes:**
  - `200 OK`: Menu option is valid
  - `400 Bad Request`: Please enter a valid option number...
  - `500 Internal Server Error`: Internal server error

#### 13. Check Option Accessibility
- **Method:** `GET`
- **Path:** `/api/menu-options/{optionNumber}/accessible-by/{userType}`
- **Description:** Check if a menu option is accessible by a specific user type
- **Parameters:**
  - `optionNumber` (path, required): Menu option number
  - `userType` (path, required): User type (A=Admin, R=Regular)
- **Response Codes:**
  - `200 OK`: Accessibility check completed
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `Boolean`

---

## Admin Menu Option Management API

### Tag: Admin Menu Option Management
**Description:** APIs for managing admin-specific menu options

### Endpoints

#### 1. Get All Active Admin Menu Options
- **Method:** `GET`
- **Path:** `/api/admin-menu-options/active`
- **Description:** Retrieve all active admin menu options ordered by display order
- **Response Codes:**
  - `200 OK`: Successful retrieval of admin menu options
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `List<AdminMenuOptionResponseDto>`

#### 2. Get Admin Menu Option by Number
- **Method:** `GET`
- **Path:** `/api/admin-menu-options/by-number/{optionNumber}`
- **Description:** Retrieve a specific admin menu option by its option number
- **Parameters:**
  - `optionNumber` (path, required): Admin menu option number
- **Response Codes:**
  - `200 OK`: Successful retrieval of admin menu option
  - `404 Not Found`: Admin menu option not found
  - `500 Internal Server Error`: Internal server error
- **Response Body:** `AdminMenuOptionResponseDto`

---

## Data Models

### UserResponseDto

```json
{
  "id": 1,
  "userType": "A",
  "userTypeDisplay": "Admin",
  "authenticated": true,
  "authenticatedDisplay": "Authenticated",
  "userId": "USR00001",
  "firstName": "John",
  "lastName": "Doe",
  "fullName": "John Doe",
  "isAdmin": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

### CreateUserRequestDto

```json
{
  "userType": "A",
  "authenticated": false,
  "userId": "USR00001",
  "password": "Pass1234",
  "firstName": "John",
  "lastName": "Doe"
}
```

### UpdateUserRequestDto

```json
{
  "userType": "A",
  "authenticated": true,
  "password": "NewPass1",
  "firstName": "John",
  "lastName": "Doe"
}
```

### LoginRequestDto

```json
{
  "userId": "USR00001",
  "password": "Pass1234"
}
```

### LoginResponseDto

```json
{
  "success": true,
  "message": "Login successful",
  "user": {
    "id": 1,
    "userId": "USR00001",
    "userType": "A",
    "firstName": "John",
    "lastName": "Doe",
    "fullName": "John Doe",
    "authenticated": true
  },
  "sessionToken": "session-token-123"
}
```

### UserSessionResponseDto

```json
{
  "id": 1,
  "transactionId": "CA00",
  "programName": "COADM01C",
  "fromProgram": "COSGN00C",
  "fromTransaction": "SG00",
  "programContext": 123456789,
  "reenterFlag": true,
  "toProgram": "COUSR00C",
  "programReenterFlag": "Y",
  "userType": "A",
  "fromTransactionId": "SG00",
  "userId": "USR00001",
  "sessionContext": "User: USR00001, Transaction: CA00, Program: COADM01C",
  "isAdminUser": true,
  "isReentering": true,
  "isProgramReentering": true,
  "hasCallingProgram": true,
  "hasContext": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

### MenuOptionResponseDto

```json
{
  "id": 1,
  "optionNumber": 1,
  "optionName": "User Management",
  "programName": "COUSR00C",
  "userTypeRequired": "A",
  "optionCount": 10,
  "isActive": true,
  "displayOrder": 1,
  "isAdminOnly": true,
  "isUserAccessible": false,
  "isComingSoon": false,
  "accessLevelDisplay": "Admin Only",
  "statusDisplay": "Active",
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

### AdminMenuOptionResponseDto

```json
{
  "id": 1,
  "optionNumber": 1,
  "optionName": "User List",
  "programName": "COUSR00C",
  "isActive": true,
  "displayOrder": 1,
  "isComingSoon": false,
  "statusDisplay": "Active",
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

---

## Error Codes

### HTTP Status Codes

| Status Code | Description | Common Scenarios |
|------------|-------------|------------------|
| 200 | OK | Successful GET, PUT requests |
| 201 | Created | Successful POST requests |
| 204 | No Content | Successful DELETE requests |
| 400 | Bad Request | Validation errors, invalid input |
| 401 | Unauthorized | Authentication failures |
| 403 | Forbidden | Access control violations |
| 404 | Not Found | Resource not found |
| 409 | Conflict | Duplicate resource (e.g., User ID already exists) |
| 500 | Internal Server Error | Server-side errors |

### Business Rule Error Messages

| Error Message | Business Rule | HTTP Status |
|--------------|---------------|-------------|
| "User ID can NOT be empty..." | BR001 - User ID Validation | 400 |
| "Password can NOT be empty..." | BR001 - Password Validation | 400 |
| "User Type can NOT be empty..." | BR001 - User Type Validation | 400 |
| "First Name can NOT be empty..." | BR001 - First Name Validation | 400 |
| "Last Name can NOT be empty..." | BR001 - Last Name Validation | 400 |
| "User ID already exist..." | BR001 - Unique User ID | 409 |
| "User ID NOT found..." | BR001 - User Existence Check | 404 |
| "Wrong Password. Try again ..." | BR001 - Password Match | 401 |
| "User not found. Try again ..." | BR001 - User Authentication | 401 |
| "User not authenticated - redirect to COSGN00C" | BR001 - Admin Authentication | 401 |
| "Please enter a valid option number..." | BR002 - Menu Option Validation | 400 |
| "No access - Admin Only option" | BR003 - Access Control by User Type | 403 |
| "Unable to Update User..." | General Update Error | 500 |
| "Unable to lookup User..." | General Lookup Error | 500 |

---

## Business Rules Implementation

### BR001: User Authentication Check
- **Endpoint:** `POST /api/users/login`
- **Implementation:** Verifies user ID and password match in database
- **Validations:**
  - User ID must not be empty
  - Password must not be empty
  - User must exist in database
  - Password must match stored password
- **Success:** Returns authenticated user with session token
- **Failure:** Returns appropriate error message (401 Unauthorized)

### BR002: Menu Option Validation
- **Endpoint:** `GET /api/menu-options/validate/{optionNumber}`
- **Implementation:** Validates menu option number is within valid range and active
- **Validations:**
  - Option number must not be null or zero
  - Option number must not exceed maximum options
  - Option must exist and be active
- **Success:** Returns 200 OK
- **Failure:** Returns 400 Bad Request with error message

### BR003: Access Control by User Type
- **Endpoint:** `GET /api/menu-options/for-user-type/{userType}`
- **Implementation:** Filters menu options based on user type
- **Logic:**
  - Admin users (A) can access all options
  - Regular users (R/U) can only access non-admin options
- **Success:** Returns filtered list of menu options
- **Failure:** Returns 403 Forbidden for unauthorized access

### BR004: Program Navigation
- **Endpoint:** `POST /api/sessions/{id}/transfer`
- **Implementation:** Transfers user session to target program
- **Process:**
  - Saves current program as fromProgram
  - Updates toProgram with target
  - Updates transaction IDs
  - Maintains session context
- **Success:** Returns updated session
- **Failure:** Returns 404 if session not found

### BR005: Coming Soon Feature Handling
- **Endpoint:** `GET /api/menu-options/coming-soon`
- **Implementation:** Identifies menu options with DUMMY program names
- **Logic:**
  - Options with programName starting with "DUMMY" are coming soon
  - Returns list of coming soon options
  - Status display shows "Coming Soon"
- **Success:** Returns list of coming soon options

### BR007: Initial Screen Display
- **Implementation:** Handled by frontend using active menu options endpoint
- **Endpoint:** `GET /api/menu-options/active`
- **Process:**
  - Retrieves all active menu options
  - Orders by display order
  - Frontend displays menu without processing input on first entry

### BR010: Option Input Normalization
- **Implementation:** Service layer method `normalizeOptionInput()`
- **Process:**
  - Removes trailing spaces from input
  - Validates numeric format
  - Returns normalized option number
- **Used by:** Menu option validation endpoints

---

## Authentication Flow

### Login Sequence

1. **User submits credentials**
   - `POST /api/users/login`
   - Body: `{ "userId": "USR00001", "password": "Pass1234" }`

2. **System validates credentials**
   - Checks user exists
   - Verifies password match
   - Updates authenticated flag

3. **System returns response**
   - Success: `{ "success": true, "user": {...}, "sessionToken": "..." }`
   - Failure: `{ "success": false, "message": "Wrong Password. Try again ..." }`

4. **Frontend stores session**
   - Saves session token
   - Stores user information
   - Redirects to appropriate menu

### Admin Access Check Sequence

1. **User attempts admin access**
   - `GET /api/users/by-user-id/{userId}/admin-check`

2. **System validates admin status**
   - Checks user is authenticated
   - Verifies user type is "A"

3. **System returns response**
   - Success: 200 OK (user is admin)
   - Failure: 401 Unauthorized (not admin or not authenticated)

---

## Pagination

All list endpoints support pagination using Spring Data's `Pageable` interface:

**Query Parameters:**
- `page`: Page number (0-indexed, default: 0)
- `size`: Number of items per page (default: 20)
- `sort`: Sort criteria (e.g., `firstName,asc`)

**Response Format:**
```json
{
  "content": [...],
  "pageable": {
    "pageNumber": 0,
    "pageSize": 20,
    "sort": {...}
  },
  "totalPages": 5,
  "totalElements": 100,
  "last": false,
  "first": true,
  "numberOfElements": 20
}
```

---

## Security Considerations

1. **Password Storage:** Passwords are currently stored in plain text. In production, implement password hashing (BCrypt).
2. **Session Management:** Implement JWT or session-based authentication for production.
3. **HTTPS:** All endpoints should be accessed over HTTPS in production.
4. **Rate Limiting:** Implement rate limiting on authentication endpoints.
5. **Input Validation:** All inputs are validated using Bean Validation annotations.
6. **SQL Injection:** Protected by JPA/Hibernate parameterized queries.

---

## Testing the API

### Using cURL

**Login:**
```bash
curl -X POST http://localhost:8080/api/users/login \
  -H "Content-Type: application/json" \
  -d '{"userId":"USR00001","password":"Pass1234"}'
```

**Get All Users:**
```bash
curl -X GET http://localhost:8080/api/users?page=0&size=20
```

**Create User:**
```bash
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "userType":"A",
    "authenticated":false,
    "userId":"USR00002",
    "password":"Pass5678",
    "firstName":"Jane",
    "lastName":"Smith"
  }'
```

### Using Swagger UI

Access the interactive API documentation at:
- **URL:** `http://localhost:8080/swagger-ui.html`
- **OpenAPI JSON:** `http://localhost:8080/v3/api-docs`

---

## Database Schema

### Tables

1. **users** - User accounts and authentication
2. **user_sessions** - Active user sessions and program context
3. **menu_options** - Available menu options for all users
4. **admin_menu_options** - Admin-specific menu options

### Relationships

- `user_sessions.user_id` → `users.user_id` (Many-to-One)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2024-01-15 | Initial API release with User Management, Session Management, and Menu Options |

---

## Support and Contact

For API support, please contact the development team or refer to the project documentation.

**Generated:** 2024-01-15  
**API Version:** 1.0.0  
**Framework:** Spring Boot 3.5.5  
**Java Version:** 21
