import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { optionNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/option-number/${params.optionNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching admin menu option by option number:', error);
    return NextResponse.json({ error: 'Failed to fetch admin menu option' }, { status: 500 });
  }
}
