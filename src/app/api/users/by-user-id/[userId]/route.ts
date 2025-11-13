import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/users/by-user-id/${params.userId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching user by userId:', error);
    return NextResponse.json({ error: 'Failed to fetch user' }, { status: 500 });
  }
}
