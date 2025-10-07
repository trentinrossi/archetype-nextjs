import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/user';

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
  }
];

// GET /api/users/[userId] - Get user by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    
    if (!userId || userId.length > 8) {
      return NextResponse.json(
        { error: 'Invalid user ID' },
        { status: 400 }
      );
    }

    const user = mockUsers.find(u => u.userId === userId);
    
    if (!user) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }

    return NextResponse.json(user);

  } catch (error) {
    console.error('Get user error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

// PUT /api/users/[userId] - Update user
export async function PUT(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();
    const body: UpdateUserSecurityRequest = await request.json();

    if (!userId || userId.length > 8) {
      return NextResponse.json(
        { error: 'Invalid user ID' },
        { status: 400 }
      );
    }

    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    
    if (userIndex === -1) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }

    // Validate optional fields
    if (body.password && body.password.length > 8) {
      return NextResponse.json(
        { error: 'Password must be 8 characters or less' },
        { status: 400 }
      );
    }

    if (body.userType && !['ADMIN', 'GENERAL'].includes(body.userType)) {
      return NextResponse.json(
        { error: 'User type must be ADMIN or GENERAL' },
        { status: 400 }
      );
    }

    // Update user
    const currentUser = mockUsers[userIndex];
    const updatedUser: UserSecurityDTO = {
      ...currentUser,
      password: body.password || currentUser.password,
      userType: body.userType || currentUser.userType,
      programName: body.programName || currentUser.programName,
      transactionId: body.transactionId || currentUser.transactionId,
      active: body.active !== undefined ? body.active : currentUser.active,
      updatedAt: new Date().toISOString(),
      userTypeDisplayName: (body.userType || currentUser.userType) === 'ADMIN' ? 'Administrator' : 'General User',
      redirectProgram: (body.userType || currentUser.userType) === 'ADMIN' ? 'COADM01C' : 'COMEN01C'
    };

    mockUsers[userIndex] = updatedUser;

    return NextResponse.json(updatedUser);

  } catch (error) {
    console.error('Update user error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

// DELETE /api/users/[userId] - Delete user
export async function DELETE(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const userId = params.userId.toUpperCase();

    if (!userId || userId.length > 8) {
      return NextResponse.json(
        { error: 'Invalid user ID' },
        { status: 400 }
      );
    }

    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    
    if (userIndex === -1) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }

    // Remove user from array
    mockUsers.splice(userIndex, 1);

    return NextResponse.json(null, { status: 204 });

  } catch (error) {
    console.error('Delete user error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}