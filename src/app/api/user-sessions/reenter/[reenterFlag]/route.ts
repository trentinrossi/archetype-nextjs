import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { reenterFlag: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/user-sessions/reenter/${params.reenterFlag}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching user sessions by reenter flag:', error);
    return NextResponse.json({ error: 'Failed to fetch user sessions' }, { status: 500 });
  }
}
