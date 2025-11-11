import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/${params.id}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching admin menu option:', error);
    return NextResponse.json({ error: 'Failed to fetch admin menu option' }, { status: 500 });
  }
}

export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/${params.id}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating admin menu option:', error);
    return NextResponse.json({ error: 'Failed to update admin menu option' }, { status: 500 });
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/admin-menu-options/${params.id}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Admin menu option deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting admin menu option:', error);
    return NextResponse.json({ error: 'Failed to delete admin menu option' }, { status: 500 });
  }
}
