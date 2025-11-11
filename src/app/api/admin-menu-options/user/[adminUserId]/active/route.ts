import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { adminUserId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/user/${params.adminUserId}/active`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching active admin menu options by user:', error);
    return NextResponse.json({ error: 'Failed to fetch active admin menu options' }, { status: 500 });
  }
}
