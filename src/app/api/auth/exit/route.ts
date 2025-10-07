import { NextRequest, NextResponse } from 'next/server';

export async function POST(request: NextRequest) {
  try {
    // Clear session cookie
    const response = NextResponse.json(
      {
        success: true,
        message: 'Thank you for using CardDemo Application',
      },
      { status: 200 }
    );

    // Remove session cookie
    response.cookies.delete('user-session');

    return response;

  } catch (error) {
    console.error('Exit error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
      },
      { status: 500 }
    );
  }
}