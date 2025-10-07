// src/app/api/users/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, CreateUserSecurityRequest, UserSecurityListResponse } from '@/types/user-security';

// Mock user database - In production, this would be replaced with actual database calls
let mockUsers: Record<string, UserSecurityDTO> = {
  'ADMIN001': {
    userId: 'ADMIN001',
    userName: 'System Administrator',
    userType: 'ADMIN',
    userStatus: 'ACTIVE',
    passwordExpired: false,
    failedSignonAttempts: 0,
    maxFailedAttempts: 3,
    accountLocked: false,
    securityLevel: 9,
    userGroup: 'ADMINISTRATORS',
    department: 'IT',
    email: 'admin@carddemo.com',
    createdDate: '2024-01-01',
    createdTime: '09:00:00',
    createdBy: 'SYSTEM',
    profileData: {
      firstName: 'System',
      lastName: 'Administrator',
      displayName: 'Admin',
      title: 'System Administrator'
    },
    permissions: [],
    roles: []
  },
  'USER001': {
    userId: 'USER001',
    userName: 'General User',
    userType: 'USER',
    userStatus: 'ACTIVE',
    passwordExpired: false,
    failedSignonAttempts: 0,
    maxFailedAttempts: 3,
    accountLocked: false,
    securityLevel: 1,
    userGroup: 'USERS',
    department: 'GENERAL',
    email: 'user@carddemo.com',
    createdDate: '2024-01-01',
    createdTime: '09:00:00',
    createdBy: 'ADMIN001',
    profileData: {
      firstName: 'General',
      lastName: 'User',
      displayName: 'User',
      title: 'General User'
    },
    permissions: [],
    roles: []
  }
};

// Mock password storage
let mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123'
};

// GET /api/users - List all users with pagination
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = parseInt(searchParams.get('page') || '0');
    const size = parseInt(searchParams.get('size') || '20');
    const sort = searchParams.get('sort') || 'userId';

    // Get all users
    const allUsers = Object.values(mockUsers);
    
    // Apply sorting
    allUsers.sort((a, b) => {
      const aValue = a[sort as keyof UserSecurityDTO] as string;
      const bValue = b[sort as keyof UserSecurityDTO] as string;
      return aValue.localeCompare(bValue);
    });

    // Apply pagination
    const startIndex = page * size;
    const endIndex = startIndex + size;
    const paginatedUsers = allUsers.slice(startIndex, endIndex);

    const response: UserSecurityListResponse = {
      users: paginatedUsers,
      totalCount: allUsers.length,
      pageSize: size,
      currentPage: page,
      totalPages: Math.ceil(allUsers.length / size),
      hasNextPage: endIndex < allUsers.length,
      hasPreviousPage: page > 0
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Get users error:', error);
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: 'Failed to retrieve users',
        statusCode: 500,
        timestamp: new Date().toISOString(),
        path: '/api/users'
      },
      { status: 500 }
    );
  }
}

// POST /api/users - Create a new user
export async function POST(request: NextRequest) {
  try {
    const body: CreateUserSecurityRequest = await request.json();

    // Validate required fields
    if (!body.userId || !body.userName || !body.password || !body.userType) {
      return NextResponse.json(
        {
          error: 'Validation Error',
          message: 'Missing required fields',
          statusCode: 400,
          timestamp: new Date().toISOString(),
          path: '/api/users',
          details: [
            ...(body.userId ? [] : [{ field: 'userId', code: 'REQUIRED', message: 'User ID is required', severity: 'ERROR' }]),
            ...(body.userName ? [] : [{ field: 'userName', code: 'REQUIRED', message: 'User Name is required', severity: 'ERROR' }]),
            ...(body.password ? [] : [{ field: 'password', code: 'REQUIRED', message: 'Password is required', severity: 'ERROR' }]),
            ...(body.userType ? [] : [{ field: 'userType', code: 'REQUIRED', message: 'User Type is required', severity: 'ERROR' }])
          ]
        },
        { status: 400 }
      );
    }

    // Validate password confirmation
    if (body.password !== body.confirmPassword) {
      return NextResponse.json(
        {
          error: 'Validation Error',
          message: 'Password confirmation does not match',
          statusCode: 400,
          timestamp: new Date().toISOString(),
          path: '/api/users',
          details: [
            { field: 'confirmPassword', code: 'MISMATCH', message: 'Password confirmation must match password', severity: 'ERROR' }
          ]
        },
        { status: 400 }
      );
    }

    const userId = body.userId.toUpperCase();

    // Check if user already exists
    if (mockUsers[userId]) {
      return NextResponse.json(
        {
          error: 'Conflict',
          message: 'User already exists',
          statusCode: 409,
          timestamp: new Date().toISOString(),
          path: '/api/users'
        },
        { status: 409 }
      );
    }

    // Create new user
    const now = new Date();
    const newUser: UserSecurityDTO = {
      userId: userId,
      userName: body.userName,
      userType: body.userType,
      userStatus: body.userStatus || 'ACTIVE',
      passwordExpired: false,
      failedSignonAttempts: 0,
      maxFailedAttempts: body.maxFailedAttempts || 3,
      accountLocked: false,
      securityLevel: body.securityLevel,
      userGroup: body.userGroup,
      department: body.department,
      email: body.email,
      phoneNumber: body.phoneNumber,
      createdDate: now.toISOString().split('T')[0],
      createdTime: now.toTimeString().split(' ')[0],
      createdBy: 'SYSTEM', // In production, this would be the current user
      profileData: body.profileData,
      permissions: [],
      roles: []
    };

    // Store user and password
    mockUsers[userId] = newUser;
    mockPasswords[userId] = body.password;

    return NextResponse.json(newUser, { status: 201 });

  } catch (error) {
    console.error('Create user error:', error);
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: 'Failed to create user',
        statusCode: 500,
        timestamp: new Date().toISOString(),
        path: '/api/users'
      },
      { status: 500 }
    );
  }
}