// src/app/api/users/[id]/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { 
  UserSecurityDTO, 
  UpdateUserSecurityRequest,
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

// Find user by ID (case-insensitive)
function findUserById(userId: string) {
  return MOCK_USERS.find(user => 
    user.id.toUpperCase() === userId.toUpperCase()
  );
}

// Validate user update data
function validateUpdateUserData(data: UpdateUserSecurityRequest): ValidationError[] {
  const errors: ValidationError[] = [];

  // Validate firstName (SEC-USR-FNAME: 20 chars) - only if provided
  if (data.firstName !== undefined) {
    if (!data.firstName || data.firstName.trim().length === 0) {
      errors.push({ field: 'firstName', message: 'First Name can NOT be empty...' });
    } else if (data.firstName.trim().length > 20) {
      errors.push({ field: 'firstName', message: 'First name must not exceed 20 characters' });
    } else if (!/^[A-Za-z\s'-]+$/.test(data.firstName.trim())) {
      errors.push({ field: 'firstName', message: 'First name must contain only letters, spaces, hyphens, and apostrophes' });
    }
  }

  // Validate lastName (SEC-USR-LNAME: 20 chars) - only if provided
  if (data.lastName !== undefined) {
    if (!data.lastName || data.lastName.trim().length === 0) {
      errors.push({ field: 'lastName', message: 'Last Name can NOT be empty...' });
    } else if (data.lastName.trim().length > 20) {
      errors.push({ field: 'lastName', message: 'Last name must not exceed 20 characters' });
    } else if (!/^[A-Za-z\s'-]+$/.test(data.lastName.trim())) {
      errors.push({ field: 'lastName', message: 'Last name must contain only letters, spaces, hyphens, and apostrophes' });
    }
  }

  // Validate userType (SEC-USR-TYPE: 1 char) - only if provided
  if (data.userType !== undefined) {
    if (!data.userType || data.userType.trim().length === 0) {
      errors.push({ field: 'userType', message: 'User Type can NOT be empty...' });
    } else if (!Object.values(UserType).includes(data.userType as UserType)) {
      errors.push({ 
        field: 'userType', 
        message: `User type must be one of: ${Object.values(UserType).join(', ')}` 
      });
    }
  }

  return errors;
}

// GET /api/users/[id] - Get user by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const userId = params.id;

    if (!userId || userId.trim().length === 0) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID is required',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    if (userId.length > 8) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID must not exceed 8 characters',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    const user = findUserById(userId);

    if (!user) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID NOT found...',
          timestamp: new Date().toISOString()
        },
        { status: 404 }
      );
    }

    const userDTO = transformToDTO(user);

    return NextResponse.json(
      {
        success: true,
        data: userDTO,
        timestamp: new Date().toISOString()
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Get user by ID API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Unable to lookup User...',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}

// PUT /api/users/[id] - Update user (COUSR02C business logic)
export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const userId = params.id;
    const body: UpdateUserSecurityRequest = await request.json();

    if (!userId || userId.trim().length === 0) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID can NOT be empty...',
          errors: [{ field: 'userId', message: 'User ID can NOT be empty...' }]
        },
        { status: 400 }
      );
    }

    // Validate input following COUSR02C business rules
    const validationErrors = validateUpdateUserData(body);
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

    const userIndex = MOCK_USERS.findIndex(user => 
      user.id.toUpperCase() === userId.toUpperCase()
    );

    if (userIndex === -1) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID NOT found...',
          errors: [{ field: 'userId', message: 'User ID NOT found...' }]
        },
        { status: 404 }
      );
    }

    const existingUser = MOCK_USERS[userIndex];
    let hasChanges = false;

    // Check for modifications and update fields
    const updatedUser = { ...existingUser };

    if (body.firstName !== undefined && body.firstName.trim() !== existingUser.firstName) {
      updatedUser.firstName = body.firstName.trim();
      hasChanges = true;
    }

    if (body.lastName !== undefined && body.lastName.trim() !== existingUser.lastName) {
      updatedUser.lastName = body.lastName.trim();
      hasChanges = true;
    }

    if (body.userType !== undefined && body.userType.toUpperCase() !== existingUser.userType) {
      updatedUser.userType = body.userType.toUpperCase();
      hasChanges = true;
    }

    if (body.isActive !== undefined && body.isActive !== existingUser.isActive) {
      updatedUser.isActive = body.isActive;
      hasChanges = true;
    }

    // Following COUSR02C logic: check if any changes were made
    if (!hasChanges) {
      return NextResponse.json(
        {
          success: false,
          message: 'Please modify to update...',
          timestamp: new Date().toISOString()
        },
        { status: 400 }
      );
    }

    // Update the user
    updatedUser.updatedAt = new Date().toISOString();
    MOCK_USERS[userIndex] = updatedUser;

    const userDTO = transformToDTO(updatedUser);

    return NextResponse.json(
      {
        success: true,
        data: userDTO,
        message: `User ${userId.toUpperCase()} has been updated...`,
        timestamp: new Date().toISOString()
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Update user API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Unable to Update User...',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}

// DELETE /api/users/[id] - Delete user (COUSR03C business logic)
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const userId = params.id;

    if (!userId || userId.trim().length === 0) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID can NOT be empty...',
          errors: [{ field: 'userId', message: 'User ID can NOT be empty...' }]
        },
        { status: 400 }
      );
    }

    const userIndex = MOCK_USERS.findIndex(user => 
      user.id.toUpperCase() === userId.toUpperCase()
    );

    if (userIndex === -1) {
      return NextResponse.json(
        {
          success: false,
          message: 'User ID NOT found...',
          errors: [{ field: 'userId', message: 'User ID NOT found...' }]
        },
        { status: 404 }
      );
    }

    // Remove user from mock database
    const deletedUser = MOCK_USERS.splice(userIndex, 1)[0];

    return NextResponse.json(
      {
        success: true,
        message: `User ${deletedUser.id} has been deleted...`,
        timestamp: new Date().toISOString()
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Delete user API error:', error);
    
    return NextResponse.json(
      {
        success: false,
        message: 'Unable to Update User...',
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
      'Access-Control-Allow-Methods': 'GET, PUT, DELETE, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, Authorization',
    },
  });
}