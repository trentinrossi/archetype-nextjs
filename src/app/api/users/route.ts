import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, CreateUserSecurityRequest, PaginationParams } from '@/types/user';

// Mock user database - in real implementation, this would be a database
let mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    password: 'ADMIN123',
    userType: 'ADMIN',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    userTypeDisplayName: 'Administrator',
    redirectProgram: 'COADM01C',
    canAuthenticate: true
  },
  {
    userId: 'USER001',
    password: 'USER123',
    userType: 'GENERAL',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    userTypeDisplayName: 'General User',
    redirectProgram: 'COMEN01C',
    canAuthenticate: true
  },
  {
    userId: 'USER002',
    password: 'PASS123',
    userType: 'GENERAL',
    programName: 'COSGN00C',
    transactionId: 'CC00',
    active: true,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    userTypeDisplayName: 'General User',
    redirectProgram: 'COMEN01C',
    canAuthenticate: true
  }
];

// GET /api/users - Get all users with pagination
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = parseInt(searchParams.get('page') || '0');
    const size = parseInt(searchParams.get('size') || '20');
    const sort = searchParams.get('sort') || 'userId';

    // Validate pagination parameters
    if (page < 0 || size <= 0 || size > 100) {
      return NextResponse.json(
        { error: 'Invalid pagination parameters' },
        { status: 400 }
      );
    }

    // Sort users
    const sortedUsers = [...mockUsers].sort((a, b) => {
      if (sort === 'userId') return a.userId.localeCompare(b.userId);
      if (sort === 'userType') return a.userType.localeCompare(b.userType);
      return 0;
    });

    // Apply pagination
    const startIndex = page * size;
    const endIndex = startIndex + size;
    const paginatedUsers = sortedUsers.slice(startIndex, endIndex);

    return NextResponse.json({
      users: paginatedUsers,
      pagination: {
        page,
        size,
        total: mockUsers.length,
        totalPages: Math.ceil(mockUsers.length / size)
      }
    });

  } catch (error) {
    console.error('Get users error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

// POST /api/users - Create a new user
export async function POST(request: NextRequest) {
  try {
    const body: CreateUserSecurityRequest = await request.json();

    // Validate required fields
    if (!body.userId || body.userId.trim() === '') {
      return NextResponse.json(
        { error: 'User ID is required' },
        { status: 400 }
      );
    }

    if (!body.password || body.password.trim() === '') {
      return NextResponse.json(
        { error: 'Password is required' },
        { status: 400 }
      );
    }

    if (!body.userType) {
      return NextResponse.json(
        { error: 'User type is required' },
        { status: 400 }
      );
    }

    // Validate field lengths
    if (body.userId.length > 8) {
      return NextResponse.json(
        { error: 'User ID must be 8 characters or less' },
        { status: 400 }
      );
    }

    if (body.password.length > 8) {
      return NextResponse.json(
        { error: 'Password must be 8 characters or less' },
        { status: 400 }
      );
    }

    // Validate user type
    if (!['ADMIN', 'GENERAL'].includes(body.userType)) {
      return NextResponse.json(
        { error: 'User type must be ADMIN or GENERAL' },
        { status: 400 }
      );
    }

    // Check if user already exists
    const existingUser = mockUsers.find(u => u.userId === body.userId.toUpperCase());
    if (existingUser) {
      return NextResponse.json(
        { error: 'User already exists' },
        { status: 409 }
      );
    }

    // Create new user
    const newUser: UserSecurityDTO = {
      userId: body.userId.toUpperCase(),
      password: body.password,
      userType: body.userType,
      programName: body.programName || 'COSGN00C',
      transactionId: body.transactionId || 'CC00',
      active: body.active !== undefined ? body.active : true,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      userTypeDisplayName: body.userType === 'ADMIN' ? 'Administrator' : 'General User',
      redirectProgram: body.userType === 'ADMIN' ? 'COADM01C' : 'COMEN01C',
      canAuthenticate: true
    };

    mockUsers.push(newUser);

    return NextResponse.json(newUser, { status: 201 });

  } catch (error) {
    console.error('Create user error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}