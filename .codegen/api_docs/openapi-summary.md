# CardDemo Admin Menu - API Documentation

## Overview

This document provides a comprehensive overview of all REST API endpoints generated for the **User Access and Security Management** macro-functionality of the CardDemo Admin Menu application.

**Base URL:** `http://localhost:8080`

**Technology Stack:**
- Spring Boot 3.5.5
- Java 21
- PostgreSQL Database
- JPA/Hibernate
- Flyway Migrations

---

## Table of Contents

1. [Admin User Management APIs](#admin-user-management-apis)
2. [Admin Menu Option Management APIs](#admin-menu-option-management-apis)
3. [User Session Management APIs](#user-session-management-apis)
4. [Data Models](#data-models)
5. [Error Handling](#error-handling)

---

## Admin User Management APIs

**Base Path:** `/api/admin-users`

### 1. Get All Admin Users
- **Endpoint:** `GET /api/admin-users`
- **Description:** Retrieve a paginated list of all admin users
- **Query Parameters:**
  - `page` (optional, default: 0) - Page number
  - `size` (optional, default: 20) - Page size
  - `sort` (optional) - Sort criteria
- **Response:** `200 OK` - Page of AdminUserResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request parameters
  - `500 Internal Server Error`

### 2. Get Admin User by ID
- **Endpoint:** `GET /api/admin-users/{userId}`
- **Description:** Retrieve a specific admin user by their user ID
- **Path Parameters:**
  - `userId` (string, max 8 chars) - Unique identifier for the admin user
- **Response:** `200 OK` - AdminUserResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 3. Create Admin User
- **Endpoint:** `POST /api/admin-users`
- **Description:** Create a new admin user in the system
- **Request Body:** CreateAdminUserRequestDto
  ```json
  {
    "userId": "ADMIN001",
    "authenticationStatus": true
  }
  ```
- **Response:** `201 Created` - AdminUserResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data or user already exists
  - `500 Internal Server Error`

### 4. Update Admin User
- **Endpoint:** `PUT /api/admin-users/{userId}`
- **Description:** Update admin user details by user ID
- **Path Parameters:**
  - `userId` (string) - User ID to update
- **Request Body:** UpdateAdminUserRequestDto
  ```json
  {
    "authenticationStatus": false
  }
  ```
- **Response:** `200 OK` - AdminUserResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 5. Delete Admin User
- **Endpoint:** `DELETE /api/admin-users/{userId}`
- **Description:** Delete an admin user by user ID
- **Path Parameters:**
  - `userId` (string) - User ID to delete
- **Response:** `204 No Content`
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 6. Authenticate Admin User
- **Endpoint:** `POST /api/admin-users/{userId}/authenticate`
- **Description:** Authenticate an admin user and set their authentication status to true
- **Path Parameters:**
  - `userId` (string) - User ID to authenticate
- **Response:** `200 OK` - AdminUserResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error` - User already authenticated

### 7. Deauthenticate Admin User
- **Endpoint:** `POST /api/admin-users/{userId}/deauthenticate`
- **Description:** Deauthenticate an admin user and set their authentication status to false
- **Path Parameters:**
  - `userId` (string) - User ID to deauthenticate
- **Response:** `200 OK` - AdminUserResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error` - User already deauthenticated

### 8. Check Authentication Status
- **Endpoint:** `GET /api/admin-users/{userId}/is-authenticated`
- **Description:** Check if an admin user is currently authenticated
- **Path Parameters:**
  - `userId` (string) - User ID to check
- **Response:** `200 OK` - Boolean (true/false)
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 9. Get All Authenticated Admin Users
- **Endpoint:** `GET /api/admin-users/authenticated`
- **Description:** Retrieve a list of all admin users who are currently authenticated
- **Response:** `200 OK` - List of AdminUserResponseDto
- **Error Responses:**
  - `500 Internal Server Error`

### 10. Get All Unauthenticated Admin Users
- **Endpoint:** `GET /api/admin-users/unauthenticated`
- **Description:** Retrieve a list of all admin users who are not currently authenticated
- **Response:** `200 OK` - List of AdminUserResponseDto
- **Error Responses:**
  - `500 Internal Server Error`

### 11. Deauthenticate All Admin Users
- **Endpoint:** `POST /api/admin-users/deauthenticate-all`
- **Description:** Deauthenticate all admin users in the system
- **Response:** `200 OK`
- **Error Responses:**
  - `500 Internal Server Error`

---

## Admin Menu Option Management APIs

**Base Path:** `/api/admin-menu-options`

### 1. Get All Admin Menu Options
- **Endpoint:** `GET /api/admin-menu-options`
- **Description:** Retrieve a paginated list of all administrative menu options
- **Query Parameters:**
  - `page` (optional, default: 0) - Page number
  - `size` (optional, default: 20) - Page size
  - `sort` (optional) - Sort criteria
- **Response:** `200 OK` - Page of AdminMenuOptionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request parameters
  - `500 Internal Server Error`

### 2. Get Admin Menu Option by ID
- **Endpoint:** `GET /api/admin-menu-options/{id}`
- **Description:** Retrieve a specific admin menu option by its ID
- **Path Parameters:**
  - `id` (long) - Menu option ID
- **Response:** `200 OK` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

### 3. Get All Active Admin Menu Options
- **Endpoint:** `GET /api/admin-menu-options/active`
- **Description:** Retrieve all administrative menu options that are currently active
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto (sorted by option number)
- **Error Responses:**
  - `500 Internal Server Error`

### 4. Get All Inactive Admin Menu Options
- **Endpoint:** `GET /api/admin-menu-options/inactive`
- **Description:** Retrieve all administrative menu options that are currently inactive
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto (sorted by option number)
- **Error Responses:**
  - `500 Internal Server Error`

### 5. Get Admin Menu Option by Option Number
- **Endpoint:** `GET /api/admin-menu-options/option-number/{optionNumber}`
- **Description:** Retrieve a specific admin menu option by its sequential option number
- **Path Parameters:**
  - `optionNumber` (integer, 0-99) - Sequential option number
- **Response:** `200 OK` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

### 6. Get Admin Menu Options by Program Name
- **Endpoint:** `GET /api/admin-menu-options/program/{programName}`
- **Description:** Retrieve admin menu options by their associated program name
- **Path Parameters:**
  - `programName` (string, max 8 chars) - Program name
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto
- **Error Responses:**
  - `500 Internal Server Error`

### 7. Get Admin Menu Options by Admin User
- **Endpoint:** `GET /api/admin-menu-options/user/{adminUserId}`
- **Description:** Retrieve all admin menu options accessible by a specific admin user
- **Path Parameters:**
  - `adminUserId` (string) - Admin user ID
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 8. Get Active Admin Menu Options by Admin User
- **Endpoint:** `GET /api/admin-menu-options/user/{adminUserId}/active`
- **Description:** Retrieve all active admin menu options accessible by a specific admin user
- **Path Parameters:**
  - `adminUserId` (string) - Admin user ID
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin user not found
  - `500 Internal Server Error`

### 9. Search Admin Menu Options by Name
- **Endpoint:** `GET /api/admin-menu-options/search?name={name}`
- **Description:** Search for admin menu options by option name (case-insensitive partial match)
- **Query Parameters:**
  - `name` (string) - Search term
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid search parameters
  - `500 Internal Server Error`

### 10. Get Admin Menu Options in Range
- **Endpoint:** `GET /api/admin-menu-options/range?start={start}&end={end}`
- **Description:** Retrieve admin menu options within a specified option number range
- **Query Parameters:**
  - `start` (integer) - Start option number
  - `end` (integer) - End option number
- **Response:** `200 OK` - List of AdminMenuOptionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid range parameters
  - `500 Internal Server Error`

### 11. Create Admin Menu Option
- **Endpoint:** `POST /api/admin-menu-options`
- **Description:** Create a new administrative menu option
- **Request Body:** CreateAdminMenuOptionRequestDto
  ```json
  {
    "optionNumber": 1,
    "optionName": "User Account Management",
    "programName": "COUSR00C",
    "isActive": true,
    "adminUserId": "ADMIN001"
  }
  ```
- **Response:** `201 Created` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data or option already exists
  - `500 Internal Server Error`

### 12. Update Admin Menu Option
- **Endpoint:** `PUT /api/admin-menu-options/{id}`
- **Description:** Update admin menu option details by ID
- **Path Parameters:**
  - `id` (long) - Menu option ID
- **Request Body:** UpdateAdminMenuOptionRequestDto
  ```json
  {
    "optionName": "Updated Option Name",
    "isActive": false
  }
  ```
- **Response:** `200 OK` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

### 13. Delete Admin Menu Option
- **Endpoint:** `DELETE /api/admin-menu-options/{id}`
- **Description:** Delete an admin menu option by ID
- **Path Parameters:**
  - `id` (long) - Menu option ID
- **Response:** `204 No Content`
- **Error Responses:**
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

### 14. Activate Admin Menu Option
- **Endpoint:** `POST /api/admin-menu-options/{id}/activate`
- **Description:** Activate a specific admin menu option by ID
- **Path Parameters:**
  - `id` (long) - Menu option ID
- **Response:** `200 OK` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

### 15. Deactivate Admin Menu Option
- **Endpoint:** `POST /api/admin-menu-options/{id}/deactivate`
- **Description:** Deactivate a specific admin menu option by ID
- **Path Parameters:**
  - `id` (long) - Menu option ID
- **Response:** `200 OK` - AdminMenuOptionResponseDto
- **Error Responses:**
  - `404 Not Found` - Admin menu option not found
  - `500 Internal Server Error`

---

## User Session Management APIs

**Base Path:** `/api/user-sessions`

### 1. Get All User Sessions
- **Endpoint:** `GET /api/user-sessions`
- **Description:** Retrieve a paginated list of all user sessions
- **Query Parameters:**
  - `page` (optional, default: 0) - Page number
  - `size` (optional, default: 20) - Page size
  - `sort` (optional) - Sort criteria
- **Response:** `200 OK` - Page of UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request parameters
  - `500 Internal Server Error`

### 2. Get User Session by ID
- **Endpoint:** `GET /api/user-sessions/{id}`
- **Description:** Retrieve a user session by its ID
- **Path Parameters:**
  - `id` (long) - Session ID
- **Response:** `200 OK` - UserSessionResponseDto
- **Error Responses:**
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

### 3. Get User Session by Transaction ID
- **Endpoint:** `GET /api/user-sessions/transaction/{transactionId}`
- **Description:** Retrieve a user session by transaction ID
- **Path Parameters:**
  - `transactionId` (string, max 4 chars) - Transaction ID (e.g., "CA00")
- **Response:** `200 OK` - UserSessionResponseDto
- **Error Responses:**
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

### 4. Get User Sessions by Program Name
- **Endpoint:** `GET /api/user-sessions/program/{programName}`
- **Description:** Retrieve user sessions by program name
- **Path Parameters:**
  - `programName` (string, max 8 chars) - Program name (e.g., "COADM01C")
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid program name
  - `500 Internal Server Error`

### 5. Get User Sessions by Reenter Flag
- **Endpoint:** `GET /api/user-sessions/reenter/{reenterFlag}`
- **Description:** Retrieve user sessions by reenter flag
- **Path Parameters:**
  - `reenterFlag` (boolean) - Reenter flag value (true/false)
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid reenter flag
  - `500 Internal Server Error`

### 6. Get User Sessions with Calling Context
- **Endpoint:** `GET /api/user-sessions/with-calling-context`
- **Description:** Retrieve user sessions that have calling context (from_program and from_transaction)
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `500 Internal Server Error`

### 7. Get User Sessions with Program Context
- **Endpoint:** `GET /api/user-sessions/with-program-context`
- **Description:** Retrieve user sessions that have program context
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `500 Internal Server Error`

### 8. Get User Sessions by From Program
- **Endpoint:** `GET /api/user-sessions/from-program/{fromProgram}`
- **Description:** Retrieve user sessions by from program name
- **Path Parameters:**
  - `fromProgram` (string, max 8 chars) - From program name
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid from program name
  - `500 Internal Server Error`

### 9. Get User Sessions by From Transaction
- **Endpoint:** `GET /api/user-sessions/from-transaction/{fromTransaction}`
- **Description:** Retrieve user sessions by from transaction ID
- **Path Parameters:**
  - `fromTransaction` (string, max 4 chars) - From transaction ID
- **Response:** `200 OK` - List of UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid from transaction ID
  - `500 Internal Server Error`

### 10. Create User Session
- **Endpoint:** `POST /api/user-sessions`
- **Description:** Create a new user session
- **Request Body:** CreateUserSessionRequestDto
  ```json
  {
    "transactionId": "CA00",
    "programName": "COADM01C",
    "fromProgram": "COMEN01C",
    "fromTransaction": "CM00",
    "programContext": 123456789,
    "reenterFlag": false
  }
  ```
- **Response:** `201 Created` - UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data
  - `500 Internal Server Error`

### 11. Update User Session
- **Endpoint:** `PUT /api/user-sessions/{id}`
- **Description:** Update user session details by ID
- **Path Parameters:**
  - `id` (long) - Session ID
- **Request Body:** UpdateUserSessionRequestDto
  ```json
  {
    "programName": "COADM02C",
    "reenterFlag": true
  }
  ```
- **Response:** `200 OK` - UserSessionResponseDto
- **Error Responses:**
  - `400 Bad Request` - Invalid request data
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

### 12. Delete User Session
- **Endpoint:** `DELETE /api/user-sessions/{id}`
- **Description:** Delete a user session by ID
- **Path Parameters:**
  - `id` (long) - Session ID
- **Response:** `204 No Content`
- **Error Responses:**
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

### 13. Clear Calling Context
- **Endpoint:** `POST /api/user-sessions/{id}/clear-calling-context`
- **Description:** Clear the calling context (from_program and from_transaction) for a user session
- **Path Parameters:**
  - `id` (long) - Session ID
- **Response:** `200 OK` - UserSessionResponseDto
- **Error Responses:**
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

### 14. Set Reenter Flag
- **Endpoint:** `POST /api/user-sessions/{id}/set-reenter-flag?reenterFlag={value}`
- **Description:** Set the reenter flag for a user session
- **Path Parameters:**
  - `id` (long) - Session ID
- **Query Parameters:**
  - `reenterFlag` (boolean) - New reenter flag value
- **Response:** `200 OK` - UserSessionResponseDto
- **Error Responses:**
  - `404 Not Found` - User session not found
  - `500 Internal Server Error`

---

## Data Models

### AdminUserResponseDto
```json
{
  "userId": "ADMIN001",
  "authenticationStatus": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Fields:**
- `userId` (string, max 8 chars) - Unique identifier for the admin user
- `authenticationStatus` (boolean) - Indicates if user has been authenticated as admin
- `createdAt` (timestamp) - Timestamp when the admin user was created
- `updatedAt` (timestamp) - Timestamp when the admin user was last updated

### AdminMenuOptionResponseDto
```json
{
  "id": 1,
  "optionNumber": 1,
  "optionName": "User Account Management",
  "programName": "COUSR00C",
  "isActive": true,
  "statusDisplay": "Active",
  "adminUserId": "ADMIN001",
  "optionDisplay": "01 - User Account Management",
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Fields:**
- `id` (long) - Unique identifier for the menu option
- `optionNumber` (integer, 0-99) - Sequential number identifying the menu option
- `optionName` (string, max 35 chars) - Descriptive name of the administrative function
- `programName` (string, max 8 chars) - Name of the program to execute for this option
- `isActive` (boolean) - Indicates if option is active or coming soon (dummy)
- `statusDisplay` (string) - Display status ("Active" or "Coming Soon")
- `adminUserId` (string) - ID of the admin user associated with this menu option
- `optionDisplay` (string) - Formatted option display text (e.g., "01 - User Account Management")
- `createdAt` (timestamp) - Timestamp when the menu option was created
- `updatedAt` (timestamp) - Timestamp when the menu option was last updated

### UserSessionResponseDto
```json
{
  "id": 1,
  "transactionId": "CA00",
  "programName": "COADM01C",
  "fromProgram": "COMEN01C",
  "fromTransaction": "CM00",
  "programContext": 123456789,
  "reenterFlag": false,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Fields:**
- `id` (long) - Unique identifier for the user session
- `transactionId` (string, max 4 chars) - Current transaction identifier (e.g., "CA00")
- `programName` (string, max 8 chars) - Current program name (e.g., "COADM01C")
- `fromProgram` (string, max 8 chars, optional) - Name of the program that transferred control
- `fromTransaction` (string, max 4 chars, optional) - Transaction ID of the calling program
- `programContext` (long, optional) - Context information for program state
- `reenterFlag` (boolean) - Indicates if program is being reentered after user input
- `createdAt` (timestamp) - Timestamp when the user session was created
- `updatedAt` (timestamp) - Timestamp when the user session was last updated

---

## Error Handling

All endpoints follow standard HTTP status codes and return error responses in the following format:

### Error Response Structure
```json
{
  "timestamp": "2024-01-15T10:30:00",
  "status": 400,
  "error": "Bad Request",
  "message": "Validation failed for field 'userId': User ID is required",
  "path": "/api/admin-users"
}
```

### Common HTTP Status Codes

- **200 OK** - Request succeeded
- **201 Created** - Resource created successfully
- **204 No Content** - Request succeeded with no content to return
- **400 Bad Request** - Invalid request data or validation failure
- **404 Not Found** - Requested resource not found
- **500 Internal Server Error** - Server error occurred

### Validation Rules

#### AdminUser
- `userId`: Required, max 8 characters
- `authenticationStatus`: Required, boolean value

#### AdminMenuOption
- `optionNumber`: Required, integer between 0 and 99
- `optionName`: Required, max 35 characters
- `programName`: Required, max 8 characters
- `isActive`: Required, boolean value
- `adminUserId`: Required, max 8 characters

#### UserSession
- `transactionId`: Required, max 4 characters
- `programName`: Required, max 8 characters
- `fromProgram`: Optional, max 8 characters
- `fromTransaction`: Optional, max 4 characters
- `programContext`: Optional, numeric value
- `reenterFlag`: Required, boolean value

---

## Database Schema

### Tables

#### admin_users
```sql
CREATE TABLE admin_users (
    user_id VARCHAR(8) PRIMARY KEY,
    authentication_status BOOLEAN NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);
```

#### admin_menu_options
```sql
CREATE TABLE admin_menu_options (
    id BIGSERIAL PRIMARY KEY,
    option_number INTEGER NOT NULL UNIQUE,
    option_name VARCHAR(35) NOT NULL,
    program_name VARCHAR(8) NOT NULL,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    admin_user_id VARCHAR(8) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (admin_user_id) REFERENCES admin_users(user_id) ON DELETE CASCADE
);
```

#### user_sessions
```sql
CREATE TABLE user_sessions (
    id BIGSERIAL PRIMARY KEY,
    transaction_id VARCHAR(4) NOT NULL,
    program_name VARCHAR(8) NOT NULL,
    from_program VARCHAR(8),
    from_transaction VARCHAR(4),
    program_context BIGINT,
    reenter_flag BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);
```

---

## Getting Started

### Prerequisites
- Java 21
- Maven 3.6+
- PostgreSQL database

### Running the Application
```bash
mvn spring-boot:run
```

The application will start on `http://localhost:8080`

### API Documentation
Once the application is running, you can access the interactive API documentation at:
- Swagger UI: `http://localhost:8080/swagger-ui.html`
- OpenAPI JSON: `http://localhost:8080/v3/api-docs`

---

## Notes

1. All timestamps are in ISO 8601 format
2. All endpoints support pagination where applicable using Spring Data's Pageable interface
3. The application uses optimistic locking with `@UpdateTimestamp` for concurrent updates
4. Foreign key constraints ensure referential integrity between entities
5. Indexes are created on frequently queried fields for optimal performance

---

**Generated:** 2024
**Version:** 1.0.0
**Macro-functionality:** User Access and Security Management
