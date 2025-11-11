'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { userSessionService } from '@/services/userSessionService';
import { UserSession } from '@/types/user-session';
import { Button } from '@/components/ui';

export default function UserSessionDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [session, setSession] = useState<UserSession | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchSession(Number(params.id));
    }
  }, [params.id]);

  const fetchSession = async (id: number) => {
    try {
      const data = await userSessionService.getUserSessionById(id);
      setSession(data);
    } catch (err) {
      console.error('Failed to load session:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this session?')) return;
    
    try {
      await userSessionService.deleteUserSession(Number(params.id));
      router.push('/admin/sessions');
    } catch (err) {
      alert('Failed to delete session');
      console.error(err);
    }
  };

  const handleClearCallingContext = async () => {
    try {
      await userSessionService.clearCallingContext(Number(params.id));
      fetchSession(Number(params.id));
    } catch (err) {
      alert('Failed to clear calling context');
      console.error(err);
    }
  };

  const handleToggleReenterFlag = async () => {
    if (!session) return;
    
    try {
      await userSessionService.setReenterFlag(session.id, !session.reenterFlag);
      fetchSession(Number(params.id));
    } catch (err) {
      alert('Failed to toggle reenter flag');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!session) return <div className="p-6">Session not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Session Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/admin/sessions/${session.id}/edit`)}>
            Edit
          </Button>
          <Button variant="secondary" onClick={handleToggleReenterFlag}>
            Toggle Reenter Flag
          </Button>
          <Button variant="secondary" onClick={handleClearCallingContext}>
            Clear Calling Context
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/sessions')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <label className="block text-sm font-semibold text-gray-700">Session ID</label>
          <p className="mt-1 text-gray-900 text-lg">{session.id}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Transaction ID</label>
          <p className="mt-1 text-gray-900 font-mono">{session.transactionId}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Program Name</label>
          <p className="mt-1 text-gray-900 font-mono">{session.programName}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">From Program</label>
          <p className="mt-1 text-gray-900 font-mono">{session.fromProgram || 'N/A'}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">From Transaction</label>
          <p className="mt-1 text-gray-900 font-mono">{session.fromTransaction || 'N/A'}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Program Context</label>
          <p className="mt-1 text-gray-900">{session.programContext || 'N/A'}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Reenter Flag</label>
          <p className="mt-1">
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${
              session.reenterFlag 
                ? 'bg-blue-100 text-blue-800' 
                : 'bg-gray-100 text-gray-800'
            }`}>
              {session.reenterFlag ? 'Yes' : 'No'}
            </span>
          </p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Created At</label>
          <p className="mt-1 text-gray-900">{new Date(session.createdAt).toLocaleString()}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Updated At</label>
          <p className="mt-1 text-gray-900">{new Date(session.updatedAt).toLocaleString()}</p>
        </div>
      </div>
    </div>
  );
}
