import { NextRequest, NextResponse } from 'next/server';

const BACKEND_URL = 'http://localhost:8080';

export async function POST(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const body = await request.json();
    const authHeader = request.headers.get('authorization');
    const userId = params.userId;
    
    const response = await fetch(`${BACKEND_URL}/api/users/${userId}/permissions`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader && { 'Authorization': authHeader }),
      },
      body: JSON.stringify(body),
    });

    const data = await response.json();

    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error('Validate permissions proxy error:', error);
    return NextResponse.json(
      { error: { message: 'Failed to connect to backend server' } },
      { status: 500 }
    );
  }
}
