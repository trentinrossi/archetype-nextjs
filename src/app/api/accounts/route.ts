import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts - List all accounts with pagination
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(`/api/accounts${queryString}`, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching accounts:', error);
    return NextResponse.json({ error: 'Failed to fetch accounts' }, { status: 500 });
  }
}

// POST /api/accounts - Create a new account
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/accounts', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating account:', error);
    return NextResponse.json({ error: 'Failed to create account' }, { status: 500 });
  }
}
