import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/menu-options/active', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching active menu options:', error);
    return NextResponse.json({ error: 'Failed to fetch active menu options' }, { status: 500 });
  }
}
