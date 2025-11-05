import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/widgets/:id - Get widget by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching widget:', error);
    return NextResponse.json({ error: 'Failed to fetch widget' }, { status: 500 });
  }
}

// PUT /api/widgets/:id - Update widget
export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating widget:', error);
    return NextResponse.json({ error: 'Failed to update widget' }, { status: 500 });
  }
}

// DELETE /api/widgets/:id - Delete widget
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Widget deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting widget:', error);
    return NextResponse.json({ error: 'Failed to delete widget' }, { status: 500 });
  }
}
