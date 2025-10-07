// src/app/api/users/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { 
  UserSecurityDTO, 
  CreateUserSecurityRequest, 
  PaginatedResponse,
  ValidationError,
  UserType
} from '@/types/userSecurity';

// Mock user database - In production, this would be replaced with actual database calls
let MOCK_USERS = [
  {
    id: 'ADMIN001',
    firstName: 'System',
    lastName: 'Administrator',
    password: 'ADMIN123',
    userType: 'A',
    isActive: true,
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z',
    lastLoginAt: '2024-01-15T10:30:00Z'
  },
  {
    id: 'USER001',
    firstName: 'John',
    lastName: 'Doe',
    password: 'USER123',
    userType: 'U',
    isActive: true,
    createdAt: '2024-01-02T00:00:00Z',
    updatedAt: '2024-01-02T00:00:00Z',
    lastLoginAt: '2024-01-14T14:20:00Z'
  },
  {
    id: 'USER002',
    firstName: 'Jane',
    lastName: 'Smith',
    password: 'PASS456',
    userType: 'U',
    isActive: true,
    createdAt: '2024-01-03T00:00:00Z',
    updatedAt: '2024-01-03T00:00:00Z',
    lastLoginAt: null
  },
  {
    id: 'INACTIVE',
    firstName: 'Inactive',
    lastName: 'User',
    password: 'PASS123',
    userType: 'U',
    isActive: false,
    createdAt: '2024-01-04T00:00:00Z',
    updatedAt: '2024-01-04T00:00:00Z',
    lastLoginAt: null
  },
  {
    id: 'MANAGER1',
    firstName: 'Bob',
    lastName: 'Manager',
    password: 'MGR789',
    userType: 'M',
    isActive: true,
    createdAt: '2024-01-05T00:00:00Z',
    updatedAt: '2024-01-05T00:00:00Z',
    lastLoginAt: '2024-01-13T09:15:00Z'
  }
];

// Transform user to DTO (exclude password)
function transformToDTO(user: any): UserSecurityDTO {
  const { password, ...userDTO } = user;
  return {
    ...userDTO,
    fullName: `${user.firstName} ${user.lastName}`.trim()
  };
}

// Validate user creation data
function validateCreateUserData(data: CreateUserSecurityRequest): ValidationError[] {
  const errors: ValidationError[] = [];

  // Validate firstName (SEC-USR-FNAME: 20 chars)
  if (!data.firstName || data.firstName.trim().length === 0) {
    errors.push({ field: 'firstName', message: 'First Name can NOT be empty...' });
  } else if (data.firstName.trim().length > 20) {
    errors.push({ field: 'firstName', message: 'First name must not exceed 20 characters' });
  } else if (!/^[A-Za-z\s'-]+$/.test(data.firstName.trim())) {
    errors.push({ field: 'firstName', message: 'First name must contain only letters, spaces, hyphens, and apostrophes' });
  }

  // Validate lastName (SEC-USR-LNAME: 20 chars)
  if (!data.lastName || data.lastName.trim().length === 0) {
    errors.push({ field: 'lastName', message: 'Last Name can NOT be empty...' });
  } else if (data.lastName.trim().length > 20) {
    errors.push({ field: 'lastName', message: 'Last name must not exceed 20 characters' });
  } else if (!/^[A-Za-z\s'-]+$/.test(data.lastName.trim())) {
    errors.push({ field: 'lastName', message: 'Last name must contain only letters, spaces, hyphens, and apostrophes' });
  }

  // Validate password (SEC-USR-PWD: 8 chars)
  if (!data.password || data.password.length === 0) {
    errors.push({ field: 'password', message: 'Password can NOT be empty...' });
  } else if (data.password.length < 6) {
    errors.push({ field: 'password', message: 'Password must be at least 6 characters long' });
  } else if (data.password.length > 20) {
    errors.push({ field: 'password', message: 'Password must not exceed 20 characters' });
  }

  // Validate userType (SEC-USR-TYPE: 1 char)
  if (!data.userType || data.userType.trim().length === 0) {
    errors.push({ field: 'userType', message: 'User Type can NOT be empty...' });
  } else if (!Object.values(UserType).includes(data.userType as UserType)) {
    errors.push({ 
      field: 'userType', 
      message: `User type must be one of: ${Object.values(UserType).join(', ')}` 
    });
  }

  return errors;
}

// Generate unique user ID
function generateUserId(): string {
  const timestamp = Date.now().toString().slice(-4);
  const random = Math.floor(Math.random() * 1000).toString().padStart(3, '0');
  return `USR${timestamp}${random}`.substring(0, 8);
}

// GET /api/users - Get all users with pagination and filtering
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    
    // Parse query parameters
    const page = parseInt(searchParams.get('page') || '0');
    const pageSize = parseInt(searchParams.get('size') || '10');
    const search = searchParams.get('search') || '';
    const userType = searchParams.get('userType') || '';
    const isActive = searchParams.get('isActive');
    const sortBy = searchParams.get('sortBy') || 'firstName';
    const sortOrder = searchParams.get('sortOrder') || 'asc';

    // Filter users
    let filteredUsers = [...MOCK_USERS];

    if (search) {
      const searchLower = search.toLowerCase();
      filteredUsers = filteredUsers.filter(user =>
        user.id.toLowerCase().includes(searchLower) ||
        user.firstName.toLowerCase().includes(searchLower) ||
        user.lastName.toLowerCase().includes(searchLower)
      );
    }

    if (userType) {
      filteredUsers = filteredUsers.filter(user => user.userType === userType);
    }

    if (isActive !== null && isActive !== '') {
      const activeFilter = isActive === 'true';
      filteredUsers = filteredUsers.filter(user => user.isActive === activeFilter);
    }

    // Sort users
    filteredUsers.sort((a, b) => {
      let aValue = a[sortBy as keyof typeof a];
      let bValue = b[sortBy as keyof typeof b];
      
      if (typeof aValue === 'string') aValue = aValue.toLowerCase();
      if (typeof bValue === 'string') bValue = bValue.toLowerCase();
      
      if (sortOrder === 'desc') {
        return aValue > bValue ? -1 : aValue < bValue ? 1 : 0;
      }
      return aValue < bValue ? -1 : aValue > bValue ? 1 : 0;
    });

    // Paginate
    const totalCount = filteredUsers.length;
    const totalPages = Math.ceil(totalCount / pageSize);
    const startIndex = page * pageSize;
    const endIndex = startIndex + pageSize;
    const paginatedUsers = filteredUsers.slice(startIndex, endIndex);

    // Transform to DTOs
    const userDTOs = paginatedUsers.map(transformToDTO);

    const response: PaginatedResponse<UserSecurityDTO> = {
      data: userDTOs,
      pagination: {
        page,
        pageSize,
        totalPages,
        hasNext: page < totalPages - 1,
        hasPrevious: page > 0
      },
      totalCount
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Get users API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}

// POST /api/users - Create new user (COUSR01C business logic)
export async function POST(request: NextRequest) {
  try {
    const body: CreateUserSecurityRequest = await request.json();

    // Validate input following COUSR01C business rules
    const validationErrors = validateCreateUserData(body);
    if (validationErrors.length > 0) {
      return NextResponse.json(
        {
          success: false,
          message: 'Validation failed',
          errors: validationErrors
        },
        { status: 400 }
      );
    }

    // Generate unique user ID
    let newUserId = generateUserId();
    
    // Ensure ID is unique
    while (MOCK_USERS.find(user => user.id === newUserId)) {
      newUserId = generateUserId();
    }

    // Create new user
    const newUser = {
      id: newUserId,
      firstName: body.firstName.trim(),
      lastName: body.lastName.trim(),
      password: body.password,
      userType: body.userType.toUpperCase(),
      isActive: body.isActive !== undefined ? body.isActive : true,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      lastLoginAt: null
    };

    // Add to mock database
    MOCK_USERS.push(newUser);

    // Return success response following COUSR01C pattern
    const userDTO = transformToDTO(newUser);

    return NextResponse.json(
      {
        success: true,
        data: userDTO,
        message: `User ${newUserId} has been added...`,
        timestamp: new Date().toISOString()
      },
      { status: 201 }
    );

  } catch (error) {
    console.error('Create user API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Unable to Add User...',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}

// Handle OPTIONS request for CORS
export async function OPTIONS(request: NextRequest) {
  return new NextResponse(null, {
    status: 200,
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization',
    },
  });
}