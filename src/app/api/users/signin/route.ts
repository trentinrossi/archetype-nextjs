import { NextRequest, NextResponse } from 'next/server';

const BACKEND_URL = 'http://localhost:8080';

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    
    const response = await fetch(`${BACKEND_URL}/api/users/signin`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(body),
    });

    const data = await response.json();

    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error('Signin proxy error:', error);
    return NextResponse.json(
      { error: { message: 'Failed to connect to backend server' } },
      { status: 500 }
    );
  }
}
