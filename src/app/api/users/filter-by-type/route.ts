import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const userType = searchParams.get('userType') || '';
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    
    const url = `/api/users/filter-by-type?userType=${userType}&page=${page}&size=${size}`;
    
    const response = await forwardAuthRequest(url, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error filtering users by type:', error);
    return NextResponse.json({ error: 'Failed to filter users' }, { status: 500 });
  }
}
