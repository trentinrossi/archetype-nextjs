import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/admin-menu-options - List all admin menu options
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    const queryString = `?page=${page}&size=${size}${sort ? `&sort=${sort}` : ''}`;
    const response = await forwardAuthRequest(`/api/admin-menu-options${queryString}`, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching admin menu options:', error);
    return NextResponse.json({ error: 'Failed to fetch admin menu options' }, { status: 500 });
  }
}

// POST /api/admin-menu-options - Create new admin menu option
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/admin-menu-options', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating admin menu option:', error);
    return NextResponse.json({ error: 'Failed to create admin menu option' }, { status: 500 });
  }
}
