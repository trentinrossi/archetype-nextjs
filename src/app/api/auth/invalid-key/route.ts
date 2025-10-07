import { NextRequest, NextResponse } from 'next/server';

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const { keyPressed } = body;

    return NextResponse.json(
      {
        success: false,
        message: 'Invalid key pressed',
        errorCode: 'INVALID_KEY',
        keyPressed,
      },
      { status: 400 }
    );

  } catch (error) {
    console.error('Invalid key handling error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error',
      },
      { status: 500 }
    );
  }
}