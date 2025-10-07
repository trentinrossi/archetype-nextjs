import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, UpdateUserSecurityRequest } from '@/types/user';

// Mock user database - in real implementation this would be a database
let mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    firstName: 'System',
    lastName: 'Administrator',
    userType: 'A',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
  {
    userId: 'USER001',
    firstName: 'General',
    lastName: 'User',
    userType: 'G',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
];

// Mock password storage - in real implementation this would be hashed
const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123',
};

function checkAuthentication(request: NextRequest) {
  const sessionCookie = request.cookies.get('user-session');
  
  if (!sessionCookie) {
    return null;
  }

  try {
    const sessionData = JSON.parse(sessionCookie.value);
    
    // Check if session is expired (30 minutes)
    const loginTime = new Date(sessionData.loginTime);
    const now = new Date();
    const diffMinutes = (now.getTime() - loginTime.getTime()) / (1000 * 60);
    
    if (diffMinutes > 30) {
      return null;
    }

    return sessionData;
  } catch {
    return null;
  }
}

// GET /api/users/[userId] - Get user by ID (COUSR02C lookup functionality)
export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    const userId = params.userId.toUpperCase();
    const user = mockUsers.find(u => u.userId === userId);

    if (!user) {
      return NextResponse.json(
        { message: 'User ID NOT found...' },
        { status: 404 }
      );
    }

    return NextResponse.json(
      {
        message: 'Press PF5 key to save your updates...',
        user,
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Get user error:', error);
    return NextResponse.json(
      { message: 'Unable to lookup User...' },
      { status: 500 }
    );
  }
}

// PUT /api/users/[userId] - Update user (COUSR02C update functionality)
export async function PUT(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    // Check if user is admin
    if (session.userType !== 'A') {
      return NextResponse.json(
        { message: 'Administrator access required' },
        { status: 403 }
      );
    }

    const userId = params.userId.toUpperCase();
    const body: UpdateUserSecurityRequest = await request.json();

    // Find existing user
    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    if (userIndex === -1) {
      return NextResponse.json(
        { message: 'User ID NOT found...' },
        { status: 404 }
      );
    }

    const existingUser = mockUsers[userIndex];

    // Validate input (COBOL validation rules)
    const errors: string[] = [];

    if (body.firstName !== undefined) {
      if (!body.firstName || body.firstName.trim() === '') {
        errors.push('First Name can NOT be empty...');
      } else if (body.firstName.length > 20) {
        errors.push('First Name must be 20 characters or less');
      }
    }

    if (body.lastName !== undefined) {
      if (!body.lastName || body.lastName.trim() === '') {
        errors.push('Last Name can NOT be empty...');
      } else if (body.lastName.length > 20) {
        errors.push('Last Name must be 20 characters or less');
      }
    }

    if (body.userType !== undefined) {
      if (!body.userType || (body.userType !== 'A' && body.userType !== 'G')) {
        errors.push('User Type can NOT be empty...');
      }
    }

    if (errors.length > 0) {
      return NextResponse.json(
        { message: errors[0], errors },
        { status: 400 }
      );
    }

    // Check if any changes were made
    let hasChanges = false;
    const updatedUser = { ...existingUser };

    if (body.firstName !== undefined && body.firstName.trim() !== existingUser.firstName) {
      updatedUser.firstName = body.firstName.trim();
      hasChanges = true;
    }

    if (body.lastName !== undefined && body.lastName.trim() !== existingUser.lastName) {
      updatedUser.lastName = body.lastName.trim();
      hasChanges = true;
    }

    if (body.userType !== undefined && body.userType !== existingUser.userType) {
      updatedUser.userType = body.userType;
      hasChanges = true;
    }

    if (!hasChanges) {
      return NextResponse.json(
        { message: 'Please modify to update...' },
        { status: 400 }
      );
    }

    // Update user
    updatedUser.updatedAt = new Date().toISOString();
    mockUsers[userIndex] = updatedUser;

    return NextResponse.json(
      {
        message: `User ${userId} has been updated...`,
        user: updatedUser,
      },
      { status: 200 }
    );

  } catch (error) {
    console.error('Update user error:', error);
    return NextResponse.json(
      { message: 'Unable to Update User...' },
      { status: 500 }
    );
  }
}

// DELETE /api/users/[userId] - Delete user (COUSR03C delete functionality)
export async function DELETE(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    // Check if user is admin
    if (session.userType !== 'A') {
      return NextResponse.json(
        { message: 'Administrator access required' },
        { status: 403 }
      );
    }

    const userId = params.userId.toUpperCase();

    // Find user
    const userIndex = mockUsers.findIndex(u => u.userId === userId);
    if (userIndex === -1) {
      return NextResponse.json(
        { message: 'User ID NOT found...' },
        { status: 404 }
      );
    }

    // Remove user
    mockUsers.splice(userIndex, 1);
    delete mockPasswords[userId];

    return NextResponse.json(
      { message: `User ${userId} has been deleted...` },
      { status: 200 }
    );

  } catch (error) {
    console.error('Delete user error:', error);
    return NextResponse.json(
      { message: 'Unable to Update User...' },
      { status: 500 }
    );
  }
}