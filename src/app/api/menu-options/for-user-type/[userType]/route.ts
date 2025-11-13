import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { userType: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/menu-options/for-user-type/${params.userType}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching menu options for user type:', error);
    return NextResponse.json({ error: 'Failed to fetch menu options' }, { status: 500 });
  }
}
