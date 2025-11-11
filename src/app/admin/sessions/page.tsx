'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { userSessionService } from '@/services/userSessionService';
import { UserSession } from '@/types/user-session';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function UserSessionsPage() {
  const router = useRouter();
  const [sessions, setSessions] = useState<UserSession[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchSessions = useCallback(async () => {
    try {
      setLoading(true);
      const response = await userSessionService.getUserSessions(0, 100);
      setSessions(response.content || response);
      setError(null);
    } catch (err) {
      setError('Failed to load user sessions');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchSessions();
  }, [fetchSessions]);

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this session?')) return;
    
    try {
      await userSessionService.deleteUserSession(id);
      fetchSessions();
    } catch (err) {
      alert('Failed to delete session');
      console.error(err);
    }
  };

  const handleClearCallingContext = async (id: number) => {
    try {
      await userSessionService.clearCallingContext(id);
      fetchSessions();
    } catch (err) {
      alert('Failed to clear calling context');
      console.error(err);
    }
  };

  const handleToggleReenterFlag = async (id: number, currentValue: boolean) => {
    try {
      await userSessionService.setReenterFlag(id, !currentValue);
      fetchSessions();
    } catch (err) {
      alert('Failed to toggle reenter flag');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">User Sessions</h1>
          <p className="text-gray-600 mt-1">Manage user session data and program context</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push('/admin/sessions/new')}>
            Create Session
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/menu')}>
            Back to Menu
          </Button>
        </div>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>ID</TableHead>
            <TableHead>Transaction ID</TableHead>
            <TableHead>Program Name</TableHead>
            <TableHead>From Program</TableHead>
            <TableHead>Reenter Flag</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {sessions.length === 0 ? (
            <TableRow>
              <TableCell colSpan={6} className="text-center text-gray-500 py-8">
                No sessions found
              </TableCell>
            </TableRow>
          ) : (
            sessions.map((session) => (
              <TableRow key={session.id}>
                <TableCell>
                  <div className="cursor-pointer font-semibold" onClick={() => router.push(`/admin/sessions/${session.id}`)}>
                    {session.id}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-mono" onClick={() => router.push(`/admin/sessions/${session.id}`)}>
                    {session.transactionId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-mono" onClick={() => router.push(`/admin/sessions/${session.id}`)}>
                    {session.programName}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-mono" onClick={() => router.push(`/admin/sessions/${session.id}`)}>
                    {session.fromProgram || '-'}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/sessions/${session.id}`)}>
                    <span className={`px-3 py-1 rounded-full text-xs font-semibold ${
                      session.reenterFlag 
                        ? 'bg-blue-100 text-blue-800' 
                        : 'bg-gray-100 text-gray-800'
                    }`}>
                      {session.reenterFlag ? 'Yes' : 'No'}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/admin/sessions/${session.id}/edit`);
                      }}
                    >
                      Edit
                    </Button>
                    <Button 
                      size="sm" 
                      variant="secondary"
                      onClick={(e) => {
                        e.stopPropagation();
                        handleToggleReenterFlag(session.id, session.reenterFlag);
                      }}
                    >
                      Toggle
                    </Button>
                    <Button 
                      size="sm" 
                      variant="danger" 
                      onClick={(e) => {
                        e.stopPropagation();
                        handleDelete(session.id);
                      }}
                    >
                      Delete
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>
    </div>
  );
}
