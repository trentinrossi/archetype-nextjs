import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/admin-users/unauthenticated', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching unauthenticated admin users:', error);
    return NextResponse.json({ error: 'Failed to fetch unauthenticated admin users' }, { status: 500 });
  }
}
