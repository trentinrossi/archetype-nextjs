import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { programName: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/program/${params.programName}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching admin menu options by program:', error);
    return NextResponse.json({ error: 'Failed to fetch admin menu options' }, { status: 500 });
  }
}
