import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function POST(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/admin-users/deauthenticate-all', 'POST', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'All admin users deauthenticated successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deauthenticating all admin users:', error);
    return NextResponse.json({ error: 'Failed to deauthenticate all admin users' }, { status: 500 });
  }
}
