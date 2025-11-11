import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function POST(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-users/${params.userId}/authenticate`,
      'POST',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error authenticating admin user:', error);
    return NextResponse.json({ error: 'Failed to authenticate admin user' }, { status: 500 });
  }
}
