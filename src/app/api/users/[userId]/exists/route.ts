import { NextRequest, NextResponse } from 'next/server';

const BACKEND_URL = 'http://localhost:8080';

export async function HEAD(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const authHeader = request.headers.get('authorization');
    const userId = params.userId;
    
    const response = await fetch(`${BACKEND_URL}/api/users/${userId}/exists`, {
      method: 'HEAD',
      headers: {
        ...(authHeader && { 'Authorization': authHeader }),
      },
    });

    return new NextResponse(null, { status: response.status });
  } catch (error) {
    console.error('Check user exists proxy error:', error);
    return new NextResponse(null, { status: 500 });
  }
}
